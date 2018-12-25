import json, strutils, sequtils, math, unicode

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
  result = true
  for x in scValue:
    let v = data{x.getStr}
    if v.isNil:
      # required field does not exist
      result = false
      break


proc handleEnum(scValue, data: JsonNode): bool =
  ## check whether `data` validates against elements of `enum`
  doAssert scValue.kind == JArray, " enum MUST be an array!"
  # TODO: Make a warning?
  if scValue.len == 0:
    echo "enum array SHOULD have at least 1 element!"
  #result = false
  #for x in scValue:
    #

  #  let res = handle
  result = scValue.getElems.anyIt(it == data)

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

  #else:
  #  echo "Invalid for ", scKind

proc validate*(schema: JsonNode, data: JsonNode): bool =
  result = true
  for k, v in schema:
    # check each element of the schema on the data
    let scKind = parseEnum[SchemaKind](k)
    # deal with current scKind
    let res = handleKind(scKind, v, data)
    if not res:
      echo "Handled kind ", scKind, " for v ", v, " in data ", data
      result = false
      break
    result = result and res
