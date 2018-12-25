import json, strutils, sequtils, math, unicode, re

# created according to:
# https://tools.ietf.org/pdf/draft-handrews-json-schema-validation-01.pdf

type
  SchemaKind* = enum
    skNot = "not"
    skType = "type"
    skProperties = "properties"
    skEnum = "enum"
    skRequired = "required"
    skConst = "const"
    skMultipleOf = "multipleOf"
    skMaximum = "maximum"
    skExclusiveMaximum = "exclusiveMaximum"
    skMinimum = "minimum"
    skExclusiveMinimum = "exclusiveMinimum"
    skMinLength = "minLength"
    skMaxLength = "maxLength"
    skPattern = "pattern"
    skItems = "items"
    skAdditionalItems = "additionalItems"
    skMaxItems = "maxItems"
    skMinItems = "minItems"
    skUniqueItems = "uniqueItems"
    skContains = "contains"
    skMaxProperties = "maxProperties"
    skMinProperties = "minProperties"

  TypeKind* = enum
    tyInteger = "integer"
    tyString = "string"
    tyNumber = "number"
    tyArray = "array"
    tyBoolean = "boolean"
    tyObject = "object"
    tyNull = "null"

proc handleKind(scKind: SchemaKind, scValue: JsonNode, data: JsonNode): bool
proc validate*(schema: JsonNode, data: JsonNode): bool

template minMaxMultipleProcs(scValue, data: JsonNode, op: untyped): untyped =
  ## helper template to avoid duplicate code receiving data from JsonNodes
  doAssert scValue.kind == JInt or scValue.kind == JFloat, " value of " &
    "maximum MUST be a number!"
  var
    num {.inject.}: float
    dataNum {.inject.}: float
  case data.kind
  of JInt:
    dataNum = data.getInt.float
  of JFloat:
    dataNum = data.getFloat
  else:
    # if not a number, valid
    return true
  case scValue.kind
  of JInt:
    num = scValue.getInt.float
  of JFloat:
    num = scValue.getFloat
  else:
    # guarded by assertion at proc top
    discard
  # apply operation
  op

proc handleMaximum(scValue, data: JsonNode): bool =
  ## check if value of `data` <= value of `scValue`
  minMaxMultipleProcs(scValue, data):
    result = dataNum <= num

proc handleExclusiveMaximum(scValue, data: JsonNode): bool =
  ## check if value of `data` < value of `scValue`
  minMaxMultipleProcs(scValue, data):
    result = dataNum < num

proc handleMinimum(scValue, data: JsonNode): bool =
  ## check if value of `data` >= value of `scValue`
  minMaxMultipleProcs(scValue, data):
    result = dataNum >= num

proc handleExclusiveMinimum(scValue, data: JsonNode): bool =
  ## check if value of `data` > value of `scValue`
  minMaxMultipleProcs(scValue, data):
    result = dataNum > num

proc handleMultipleOf(scValue, data: JsonNode): bool =
  ## check if value of `data` divided by `scValues` value results in an integer
  minMaxMultipleProcs(scValue, data):
    result = (dataNum / num) == (dataNum / num).trunc

proc handleMaxLength(scValue, data: JsonNode): bool =
  ## check if length of string of `data` is <= than `scValue`
  doAssert scValue.kind == JInt, " maxLength MUST be an integer!"
  doAssert scValue.getInt >= 0, " value of maxLength MUST be >= 0!"
  case data.kind
  of JString:
    result = if data.getStr.runeLen <= scValue.getInt: true else: false
  else:
    result = true

proc handleMinLength(scValue, data: JsonNode): bool =
  ## check if length of string of `data` is >= than `scValue`
  doAssert scValue.kind == JInt, " maxLength MUST be an integer!"
  doAssert scValue.getInt >= 0, " value of maxLength MUST be >= 0!"
  case data.kind
  of JString:
    result = if data.getStr.runeLen >= scValue.getInt: true else: false
  else:
    result = true

proc handleConst(scValue, data: JsonNode): bool =
  ## check if value of `data` is equal to `const` element of `scValue`
  result = scValue == data

proc handleRequired(scValue, data: JsonNode): bool =
  ## checks whether required fields in `scValue` exist in `data`
  doAssert scValue.kind == JArray, " required field MUST be an array!"
  doAssert scValue.getElems.allit(it.kind == JString), " all elements of " &
    "required array MUST be strings!"
  case data.kind
  of JObject:
    result = true
    for x in scValue:
      let v = data{x.getStr}
      if v.isNil:
        # required field does not exist
        result = false
        break
  else:
    result = true

proc handleEnum(scValue, data: JsonNode): bool =
  ## check whether `data` validates against elements of `enum`
  doAssert scValue.kind == JArray, " enum MUST be an array!"
  # TODO: Make a warning?
  if scValue.len == 0:
    echo "enum array SHOULD have at least 1 element!"
  result = scValue.getElems.anyIt(it == data)

proc handlePattern(scValue, data: JsonNode): bool =
  ## check whether string of `data` conforms to `scValue` regex pattern
  doAssert scValue.kind == JString, " pattern kind MUST be a string!"
  case data.kind
  of JString:
    let regex = re(scValue.getStr)
    let str = data.getStr
    # search for pattern in string (needed because pattern is not necessarily
    # from beginning of string!)
    let start = find(str, regex)
    if start >= 0:
      result = if match(data.getStr, regex, start): true else: false
    else:
      result = false
  else:
    result = true

proc handleItems(scValue, data: JsonNode): bool =
  ## check if all elements of `data` validate against `scValue` schema
  ## or if (scValue is array) each element of `data` is valid for
  ## the schema of `scValue` at the same index
  # TODO: assert scValue either valid schema or array of valid schemas
  let scItems = scValue[$skItems]
  let scAddItems = scValue[$skAdditionalItems]

  case scItems.kind
  of JArray:
    # check each idx
    result = true
    case data.kind
    of JArray:
      var idx = 0
      for x in scItems:
        if idx < data.len:
          result = result and (validate(x, data[idx]))
          inc idx
        else:
          break
      if data.len > scItems.len:
        # TODO: put this into a call to handleItems itself?
        # continue with additionalItems, if any
        if not scAddItems.isNil:
          case scAddItems.kind
          of JArray:
            for x in scAddItems:
              if idx < data.len:
                result = result and (validate(x, data[idx]))
                inc idx
              else:
                break
          else:
            # TODO: only validate starting from idx?
            for i in idx ..< data.len:
              result = result and (validate(scAddItems, data[idx]))
              inc idx
    else:
      # if data is not array: is valid
      result = true
  else:
    # Ignore additional Items always
    case data.kind
    of JArray:
      result = true
      for x in data:
        echo "X is ", x, " for scal ", scItems
        echo "scval kind ", scItems.kind
        result = result and (validate(scItems, x))
    else:
      result = true

proc handleMaxItems(scValue, data: JsonNode): bool =
  ## checks whether `data` is of length <= `scValue` if data is an array
  doAssert scValue.kind == JInt, " maxItems MUST be an integer!"
  doAssert scValue.getInt >= 0, " maxItems MUST be an integer >= 0!"
  case data.kind
  of JArray:
    result = if data.len <= scValue.getInt: true else: false
  else:
    result = true

proc handleMinItems(scValue, data: JsonNode): bool =
  ## checks whether `data` is of length >= `scValue` if data is an array
  doAssert scValue.kind == JInt, " minItems MUST be an integer!"
  doAssert scValue.getInt >= 0, " minItems MUST be an integer >= 0!"
  case data.kind
  of JArray:
    result = if data.len >= scValue.getInt: true else: false
  else:
    result = true

proc handleUniqueItems(scValue, data: JsonNode): bool =
  ## checks whether data array does not contain duplicates
  doAssert scValue.kind == JBool, " uniqueItems MUST be a boolean!"
  let val = scValue.getBool
  if not val:
    result = true
  else:
    case data.kind
    of JArray:
      let elems = data.getElems
      result = if elems.deduplicate.len == elems.len: true else: false
    else:
      doAssert false, "Invalid type for?! " & $scValue & " with data " & $data

proc handleContains(scValue, data: JsonNode): bool =
  ## checks whether `scValue` is equal to any element in data
  case data.kind
  of JArray:
    # check whether any is valid
    result = false
    for x in data:
      result = result or (validate(scValue, x))
      if result:
        break
  else:
    # not array is valid
    result = true

proc handleMaxProperties(scValue, data: JsonNode): bool =
  ## checks whether `data` has <= `scValue` properties
  doAssert scValue.kind == JInt, " maxProperties MUST be an integer!"
  doAssert scValue.getInt >= 0, " maxProperties MUST be >= 0!"
  case data.kind
  of JObject:
    result = true
    var count = 0
    for k, v in pairs(data):
      inc count
      if count > scValue.getInt:
        result = false
        break
  else:
    # if non object is ignored
    result = true

proc handleMinProperties(scValue, data: JsonNode): bool =
  ## checks whether `data` has <= `scValue` properties
  doAssert scValue.kind == JInt, " maxProperties MUST be an integer!"
  doAssert scValue.getInt >= 0, " maxProperties MUST be >= 0!"
  case data.kind
  of JObject:
    result = false
    var count = 0
    for k, v in pairs(data):
      inc count
      if count >= scValue.getInt:
        result = true
        break
  else:
    # if non object is ignored
    result = true

proc handleProperties(scValue, data: JsonNode): bool =
  ## check whether given `properties` schema value is true for `data`
  doAssert scValue.kind == JObject, " no type is " & $scValue.kind
  echo "SC VALUE IS ", scValue
  for k, v in pairs(scValue):
    # let scKind = parseEnum[SchemaKind]($k)
    #echo "in not ", scKind
    # check if current key in `data`
    echo "SO THERE IS ", k, " for data ", data
    let dataVal = data{k}
    if not dataVal.isNil:
      # if it exists, check if it's valid given `v`
      echo "Validating ", dataVal
      # TODO: Combine all  calls to Validate in one!!!
      result = validate(v, dataVal) #handleKind(scKind, v, data)
    else:
      # if `properties` key not found in `data` it's valid,
      # because there exist not constraints
      echo "Not found: ", k
      result = true

proc handleNot(scValue, data: JsonNode): bool =
  ## check whether given `not` schema value is true for `data`
  #doAssert scValue.kind == JObject, " no type is " & $scValue.kind
  echo "NOt in ", scValue, " for ", data
  result = false
  case scValue.kind
  of JBool:
    # if not is boolean, allow / disallow everything
    # TODO: still needed if `validates` handles JBool of schema?
    result = not scValue.getBool
  of JObject:
    for k, v in pairs(scValue):
      let scKind = parseEnum[SchemaKind]($k)
      echo "in not ", scKind
      let res = handleKind(scKind, v, data)
      if not res:
        result = true
        break
      result = result and (not res)
  else:
    doassert false, "Unsupported kind " & $scValue & " for " & $data

proc handleType(scValue, data: JsonNode): bool =
  doAssert scValue.kind == JString or scValue.kind == JArray
  echo "sc value ", scValue
  # default needs to be false. If `JArray` only *one* type needs
  # to be valid
  result = false
  case scValue.kind
  of JArray:
    # assert all vaues are string
    doAssert scValue.getElems.allIt(it.kind == JString), " all elements of " &
      "type array MUST be strings!"
    for x in scValue:
      let aaa = handleType(x, data)
      echo "AAA is ", aaa, " for type ", x, " and value ", data
      # Only one type needs to be valid, hence `or`
      result = result or aaa
      echo "Result thus ", result
  of JString:
    let tyKind = parseEnum[TypeKind](scValue.getStr)
    case tyKind
    of tyInteger:
      echo "Type is ", data.kind, " val ", data
      result = data.kind == JInt
    of tyBoolean:
      echo "bool: Type is ", data.kind, " val ", data
      result = data.kind == JBool
    of tyString:
      echo "String: Type is ", data.kind, " val ", data
      result = data.kind == JString
    of tyObject:
      echo "object: Type is ", data.kind, " val ", data
      result = data.kind == JObject
      echo "object result is ", result
    of tyArray:
      result = data.kind == JArray
    of tyNumber:
      result = data.kind == JFloat or data.kind == JInt
    of tyNull:
      result = data.kind == JNull
    #else:
    #  echo "Invalid for type ", tyKind
  else:
    echo "Invalid type ", scValue.kind, " in handleType"

proc handleKind(scKind: SchemaKind, scValue: JsonNode, data: JsonNode): bool =
  case scKind
  of skNot:
    echo "Handle not ", scValue, " for data ", data
    result = handleNot(scValue, data)
  of skType:
    echo "Handle type "
    result = handleType(scValue, data)
  of skProperties:
    echo "Handle properties"
    result = handleProperties(scValue, data)
  of skEnum:
    echo "Handle enum"
    result = handleEnum(scValue, data)
  of skRequired:
    echo "Handle required"
    result = handleRequired(scValue, data)
  of skConst:
    echo "Handle const"
    result = handleConst(scValue, data)
  of skMultipleOf:
    echo "Handle multipleOf"
    result = handleMultipleOf(scValue, data)
  of skMaximum:
    echo "Handle maximum"
    result = handleMaximum(scValue, data)
  of skExclusiveMaximum:
    echo "Handle exclusive maximum"
    result = handleExclusiveMaximum(scValue, data)
  of skMinimum:
    echo "Handle minimum"
    result = handleMinimum(scValue, data)
  of skExclusiveMinimum:
    echo "Handle exclusive minimum"
    result = handleExclusiveMinimum(scValue, data)
  of skMinLength:
    echo "Handle min length"
    result = handleMinLength(scValue, data)
  of skMaxLength:
    echo "Handle max length"
    result = handleMaxLength(scValue, data)
  of skPattern:
    echo "Handle pattern"
    result = handlePattern(scValue, data)
  of skItems:
    echo "Handle items"
    result = handleItems(scValue, data)
  of skAdditionalItems:
    echo "Handle additional items"
    echo "Additional items MUST NOT be handled individually!"
  of skMaxItems:
    echo "Handle max Items"
    result = handleMaxItems(scValue, data)
  of skMinItems:
    echo "Handle min Items"
    result = handleMinItems(scValue, data)
  of skUniqueItems:
    echo "Handle unique items"
    result = handleUniqueItems(scValue, data)
  of skContains:
    echo "Handle conatins"
    result = handleContains(scValue, data)
  of skMaxProperties:
    echo "Handle max properties"
    result = handleMaxProperties(scValue, data)
  of skMinProperties:
    echo "Handle min properties"
    result = handleMinProperties(scValue, data)
  #else:
  #  echo "Invalid for ", scKind

proc validate*(schema: JsonNode, data: JsonNode): bool =
  result = true
  echo "validating something ", schema, " TYPE ", schema.kind
  case schema.kind
  of JObject:
    for k, v in schema:
      # check each element of the schema on the data
      let scKind = parseEnum[SchemaKind](k)
      var addItems: JsonNode
      var res: bool
      case scKind
      of skItems:
        # check if additional items exists
        let addItems = schema{$skAdditionalItems}
        # build object containing only `items` and `additionalItems`
        let scArray = %* {$skItems: v, $skAdditionalItems: addItems}
        res = handleKind(scKind, scArray, data)
      of skAdditionalItems:
        # skip, because dealt with together with items
        continue
      else:
        # deal with current scKind
        res = handleKind(scKind, v, data)

      if not res:
        echo "Handled kind ", scKind, " for v ", v, " in data ", data
        result = false
        break
      result = result and res
  of JBool:
    result = schema.getBool
    if not result and
       data.kind == JArray and
       data.getElems.len == 0:
      # if schema boolean false and data is empty array, true anyways
      result = true
  else:
    doAssert false, " unsupported schema type?! " & $schema.kind & " val " & $schema
