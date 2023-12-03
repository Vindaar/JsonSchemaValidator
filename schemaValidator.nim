import json# except `$`
import strutils, sequtils, math, unicode, re, options, sets, uri, httpclient

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
    skAdditionalProperties = "additionalProperties"
    skPatternProperties = "patternProperties"
    skAllOf = "allOf"
    skAnyOf = "anyOf"
    skOneOf = "oneOf"
    skIf = "if"
    skThen = "then"
    skElse = "else"
    skPropertyNames = "propertyNames"
    skDependency = "dependencies"
    skRef = "$ref"

  TypeKind* = enum
    tyInteger = "integer"
    tyString = "string"
    tyNumber = "number"
    tyArray = "array"
    tyBoolean = "boolean"
    tyObject = "object"
    tyNull = "null"

# Global helper constant, which is set upon a call to `validate` in order to
# have access to the full schema if required
var CurrentSchema: JsonNode


#proc `$`(n: JsonNode): string
#
#proc str(n: JsonNode, visited: var HashSet[JsonNode], level = 0): string =
#  if n in visited:
#    echo "DUPLICATE!"
#    return ""
#  visited.incl n
#  case n.kind
#  of JObject:
#    result &= "{\n"
#    for k, v in n:
#      result &= $k & ": " & $v & ","
#    result &= "}\n"
#  of JArray:
#    result &= "["
#    for x in n:
#      result &= $x & ", "
#    result &= "],\n"
#  of JNull:
#    result &= "nil"
#  of JInt:
#    result &= $n.getInt
#  of JFloat:
#    result &= $n.getFloat
#  of JString:
#    result &= n.getStr
#  of JBool:
#    result &= $n.getBool
#
#proc `$`(n: JsonNode): string =
#  var visited = initSet[JsonNode]()
#  result = str(n, visited)

proc handleKind(scKind: SchemaKind, scValue: JsonNode, data: JsonNode): bool
proc validateImpl*(schema: JsonNode, data: JsonNode): bool
proc validate*(schema: JsonNode, data: JsonNode): bool

template minMaxMultipleProcs(scValue, data: JsonNode, op: untyped): untyped =
  ## helper template to avoid duplicate code receiving data from JsonNodes
  doAssert scValue.kind == JInt or scValue.kind == JFloat, " value of " &
    "minimum or maximum MUST be a number!"
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
          result = result and (validateImpl(x, data[idx]))
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
                result = result and (validateImpl(x, data[idx]))
                inc idx
              else:
                break
          else:
            # TODO: only validate starting from idx?
            for i in idx ..< data.len:
              result = result and (validateImpl(scAddItems, data[idx]))
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
        #echo "X is ", x, " for scal ", scItems
        #echo "scval kind ", scItems.kind
        result = result and (validateImpl(scItems, x))
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
      result = result or (validateImpl(scValue, x))
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

proc handlePatternProperties(scValue, data: JsonNode, matchedKeys: var HashSet[string]):
                            Option[bool] =
  ## checks whether all properties matching `scValues` key
  ## validate against its value
  ## NOTE: special return value: returns `some` if a pattern matched. The
  ## value of `some` is the validity of the match
  doAssert scValue.kind == JObject, " patternProperties MUST be an object!"
  var matched = false
  var res = false
  case data.kind
  of JObject:
    res = true
    for k, v in pairs(scValue):
      # check whether any k matches any field
      for dk, dv in pairs(data):
        # use handle pattern to check if this `dk` matches
        #echo "Handle patterN: ", k, " and ", dk
        let newMatch = handlePattern(% k, % dk)
        matched = matched or newMatch
        if newMatch:
          # check if values are valid under `v`
          matchedKeys.incl dk
          res = res and (validateImpl(scValue[k], data[dk]))
    if matched:
      result = some(res)
    else:
      #echo "Going the none route"
      result = none[bool]()
  else:
    # non objects are ignored; is valid
    result = some(true)

proc handleAdditionalProperties(scValue, data: JsonNode, matchedKeys: HashSet[string]): bool =
  ## `matchedKeys` contains all keys, which were already matched by normal properties
  ## or pattern properties
  case scValue.kind
  of JBool:
    # disallow additional properties if addProps == false
    let allowed = scValue.getBool
    #echo "Allowed ", allowed
    if allowed or matchedKeys.card == data.len:
      result = true
    else:
      case data.kind
      of JObject:
        # all keys must have been matched before
        result = false
      else: result = true
  of JObject:
    for k, v in data:
      result = validateImpl(scValue, v)# )#true
    #for k, v in pairs(scValue):
      #echo "Add Prop: ", k, " w/ val ", v, " for data: ", data
      #result = result and (validate(v, data))
  else:
    assert false, "Unsupported type for " & $scValue & " and " & $data

proc handleProperties(scValue, data: JsonNode): bool =
  ## check whether given `properties` schema value is true for `data`
  doAssert scValue.kind == JObject, " no type is " & $scValue.kind
  let
    scPatternProps = scValue{$skPatternProperties}
    scAddProps = scValue{$skAdditionalProperties}
  # TOOD: fix necessity for `var` here by changing `handleNot`
  var
    scProps = scValue{$skProperties}
  if scAddProps.isNil and scPatternProps.isNil and scProps.isNil:
    scProps = scValue
  elif scProps.isNil:
    scProps = newJObject()

  var mdata = data
  result = true #false
  var matched = false
  var matchedKeys = initHashSet[string]()
  for k, v in pairs(scProps):
    # let scKind = parseEnum[SchemaKind]($k)
    #echo "in not ", scKind
    # check if current key in `data`
    #echo "SO THERE IS ", k, " for data ", data
    let dataVal = data{k}
    if not dataVal.isNil:
      # if it exists, check if it's valid given `v`
      matched = true
      matchedKeys.incl k
      #echo "Validating ", dataVal, " with prop value ", v
      # TODO: Combine all  calls to Validate in one!!!
      let res = validateImpl(v, dataVal)
      result = result and res #handleKind(scKind, v, data)
      #echo "Res is ", res
      #echo "result ", result, " for ", v, " on ", dataVal

  # check if `patternProperties` adds additional constraints
  if not scPatternProps.isNil:
    # then check via pattern properties if nullified, so `and` both
    let optRes = (handlePatternProperties(scPatternProps, data, matchedKeys))
    if optRes.isSome:
      matched = matched or true
      result = result and optRes.get
    #echo "And result of pattern ", optRes, " so now ", result
  # or `additionalProperties`
  var addPropValid = false
  if not scAddProps.isNil and data.len != matchedKeys.card:# and not matched:
    # try additional properties if nothing matched
    #echo "DID not match ", data
    addPropValid = handleAdditionalProperties(scAddProps, data, matchedKeys)
    #echo "Validated ", result
    result = addPropValid

  # check if anything has invalidated result
  if scProps.len > 0 and
     not scPatternProps.isNil and
     not scAddProps.isNil and
     not addPropValid and
     not matched:
    #echo "Setting from ", result, ", to false"
    result = false

proc handleNot(scValue, data: JsonNode): bool =
  ## check whether given `not` schema value is true for `data`
  #doAssert scValue.kind == JObject, " no type is " & $scValue.kind
  #echo "NOt in ", scValue, " for ", data
  result = false
  case scValue.kind
  of JBool:
    # if not is boolean, allow / disallow everything
    # TODO: still needed if `validates` handles JBool of schema?
    result = not scValue.getBool
  of JObject:
    for k, v in pairs(scValue):
      let scKind = parseEnum[SchemaKind]($k)
      #echo "in not ", scKind
      let res = handleKind(scKind, v, data)
      if not res:
        result = true
        break
      result = result and (not res)
  else:
    doassert false, "Unsupported kind " & $scValue & " for " & $data

proc handleAllOf(scValue, data: JsonNode): bool =
  ## check whether all elements of `allOf` match against data
  doAssert scValue.kind == JArray, " allOf MUST be a JArray!"
  doAssert scValue.len > 0, " allOf array MUST NOT be empty!"
  result = true
  for x in scValue:
    result = result and validateImpl(x, data)

proc handleAnyOf(scValue, data: JsonNode): bool =
  ## check whether any elements of `anyOf` matches against data
  doAssert scValue.kind == JArray, " anyOf MUST be a JArray!"
  doAssert scValue.len > 0, " anyOf array MUST NOT be empty!"
  result = false
  for x in scValue:
    result = result or validateImpl(x, data)

proc handleOneOf(scValue, data: JsonNode): bool =
  ## check whether exactly ONE elements of `oneOf` matches against data
  doAssert scValue.kind == JArray, " oneOf MUST be a JArray!"
  doAssert scValue.len > 0, " oneOf array MUST NOT be empty!"
  result = false
  for x in scValue:
    let res = validateImpl(x, data)
    if res and result:
      # means matched two, not allowed
      return false
    else:
      result = result or res

proc handleIfThenElse(scValue, data: JsonNode): bool =
  ## checks whether `if-then-else` matches
  let
    scIf = scValue{$skIf}
    scThen = scValue{$skThen}
    scElse = scValue{$skElse}
  if (scThen.isNil and scElse.isNil) or
    scIf.isNil:
    # if neither `then` nor `else` present, ignore
    # also if no `if` present
    return true

  let ifRes = validateImpl(scIf, data)
  if ifRes:
    if not scThen.isNil:
      result = validateImpl(scThen, data)
    else:
      result = true
  else:
    if not scElse.isNil:
      result = validateImpl(scElse, data)
    else:
      result = true

proc handleType(scValue, data: JsonNode): bool =
  doAssert scValue.kind == JString or scValue.kind == JArray
  #echo "sc value ", scValue
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
      #echo "AAA is ", aaa, " for type ", x, " and value ", data
      # Only one type needs to be valid, hence `or`
      result = result or aaa
      #echo "Result thus ", result
  of JString:
    let tyKind = parseEnum[TypeKind](scValue.getStr)
    case tyKind
    of tyInteger:
      #echo "Type is ", data.kind, " val ", data
      result = data.kind == JInt
    of tyBoolean:
      #echo "bool: Type is ", data.kind, " val ", data
      result = data.kind == JBool
    of tyString:
      #echo "String: Type is ", data.kind, " val ", data
      result = data.kind == JString
    of tyObject:
      #echo "object: Type is ", data.kind, " val ", data
      result = data.kind == JObject
      #echo "object result is ", result
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

proc handlePropertyNames(scValue, data: JsonNode): bool =
  ## checks whether the the keys of `data` are valid under `propertyName`
  result = true
  case data.kind
  of JObject:
    for k, v in data:
      result = result and validateImpl(scValue, % k)
  else:
    result = true

proc handleDependency(scValue, data: JsonNode): bool =
  ## checks whether dependencies met for all `x` in `data`, which
  ## have a dependency in `scValue`
  doAssert scValue.kind == JObject, "value of dependencies MUST be a JObject!"
  case data.kind
  of JObject:
    result = true
    for k, v in scValue:
      case v.kind
      of JArray:
        let dataVal = data{k}
        if not dataVal.isNil:
          for x in v:
            # check all elements of `v` are in data
            doAssert x.kind == JString, " Element of `dependency` JArray MUST be JString!"
            result = result and (not data{x.getStr}.isNil)
      else:
        let dataVal = data{k}
        if not dataVal.isNil:
          result = result and validateImpl(v, data)
  else:
    # ignores everything but objects
    result = true


proc preparePropObj(schema: JsonNode): JsonNode =
  # check if properties exist
  let props = schema{$skProperties}
  # check if pattern properties exist
  let patternProps = schema{$skPatternProperties}
  # check if additional properties exists
  let addProps = schema{$skAdditionalProperties}
  # build object containing only `properties` and `additionalProperties`
  result = newJObject()
  if not props.isNil:
    result[$skProperties] = props
  if not patternProps.isNil:
    result[$skPatternProperties] = patternProps
  if not addProps.isNil:
    result[$skAdditionalProperties] = addProps

proc prepareIfThenElseObj(schema: JsonNode): JsonNode =
  # check if `if` exists
  let ifEl = schema{$skIf}
  # check if `then` exists
  let thenEl = schema{$skThen}
  # check if `else` exists
  let elseEl = schema{$skElse}
  result = newJObject()
  if not ifEl.isNil:
    result[$skIf] = ifEl
  if not thenEl.isNil:
    result[$skThen] = thenEl
  if not elseEl.isNil:
    result[$skElse] = elseEl

proc resolveReference(schema: JsonNode, link: string): (string, JsonNode) =
  ## resolves the given reference `link` in the `schema`
  result[1] = CurrentSchema#deepcopy(schema)
  let path = link.split(sep = "/")
  #echo "Path ", path
  #doAssert path[0] == "#", " Reference MUST currently be relative to local schema!"
  case path[0]
  of "#":
    # is local
    var n: JsonNode
    #echo "input ", schema, " and linnk ", link
    #echo "Path is ", path
    for i in 1 .. path.high:
      #echo "Result ", result, " for i ", i
      let curStr = path[i]
      if curStr.allIt(it.isDigit):
        result[1] = result[1][curStr.parseInt]
      else:
        # escape URI and then escape ~0 and ~1
        var decodeCurStr = curStr.decodeUrl
        # replace first `~1` by `/`, then `~0` by `~`
        decodeCurStr = decodeCurStr.multiReplace(("~1", "/"))
        decodeCurStr = decodeCurStr.multiReplace(("~0", "~"))
        result[1] = result[1][decodeCurStr]
    #echo "Final result: ", result
    result[0] = path[path.high]
  of "http:", "https:":
    # get content from URL
    let data = getContent(link)
    #echo "Data is ", data
    result[1] = data.parseJson
    CurrentSchema = data.parseJson
    #echo "PATH ", path
    result[0] = "remote"


# proc traverseNodes(schema, node: JsonNode, parents: seq[string] = @[]): JsonNode =
#   case node.kind
#   of JObject:
#     result = node
#     for k, v in node:
#       #echo " ??? ", k, " v ", v
#       let scKind = parseEnum[SchemaKind](k, skNot)
#       case scKind
#       of skRef:
#         let (key, refNode) = resolveReference(schema, v.getStr)
#         echo "Ref node is ", refNode
#         #result = deepcopy(schema)
#         #var toDel = schema
#         #for p in parents:
#         #  toDel = toDel[p]
#         #result.delete(toDel.getStr)
#         echo "Parents: ", parents
#         if parents.len == 0:
#           result = refNode
#         else:
#           result[parents[parents.high]] = refNode
#           result.delete(k)
#       else:
#         let newParents = concat(parents, @[k])
#         result = traverseNodes(schema, v, newParents)
#   else:
#     result = node

# proc resolveAllReferences(schema: JsonNode): JsonNode =
#   ## returns a copy of the given `schema` with all references already
#   ## resolved. The resulting JsonNode might be recursive!
#   #echo "SCehame before ", schema.pretty
#   result = traverseNodes(schema, schema)
  #echo "Schema after repl ", result.pretty

proc handleKind(scKind: SchemaKind, scValue: JsonNode, data: JsonNode): bool =
  case scKind
  of skNot:
    #echo "Handle not ", scValue, " for data ", data
    result = handleNot(scValue, data)
  of skType:
    #echo "Handle type "
    result = handleType(scValue, data)
  of skProperties:
    #echo "Handle properties "
    result = handleProperties(scValue, data)
  of skEnum:
    #echo "Handle enum"
    result = handleEnum(scValue, data)
  of skRequired:
    #echo "Handle required"
    result = handleRequired(scValue, data)
  of skConst:
    #echo "Handle const"
    result = handleConst(scValue, data)
  of skMultipleOf:
    #echo "Handle multipleOf"
    result = handleMultipleOf(scValue, data)
  of skMaximum:
    #echo "Handle maximum"
    result = handleMaximum(scValue, data)
  of skExclusiveMaximum:
    #echo "Handle exclusive maximum"
    result = handleExclusiveMaximum(scValue, data)
  of skMinimum:
    #echo "Handle minimum"
    result = handleMinimum(scValue, data)
  of skExclusiveMinimum:
    #echo "Handle exclusive minimum"
    result = handleExclusiveMinimum(scValue, data)
  of skMinLength:
    #echo "Handle min length"
    result = handleMinLength(scValue, data)
  of skMaxLength:
    #echo "Handle max length"
    result = handleMaxLength(scValue, data)
  of skPattern:
    #echo "Handle pattern"
    result = handlePattern(scValue, data)
  of skItems:
    #echo "Handle items"
    result = handleItems(scValue, data)
  of skAdditionalItems:
    echo "Handle additional items"
    echo "Additional items MUST NOT be handled individually!"
  of skMaxItems:
    #echo "Handle max Items"
    result = handleMaxItems(scValue, data)
  of skMinItems:
    #echo "Handle min Items"
    result = handleMinItems(scValue, data)
  of skUniqueItems:
    #echo "Handle unique items"
    result = handleUniqueItems(scValue, data)
  of skContains:
    #echo "Handle conatins"
    result = handleContains(scValue, data)
  of skMaxProperties:
    #echo "Handle max properties"
    result = handleMaxProperties(scValue, data)
  of skMinProperties:
    #echo "Handle min properties"
    result = handleMinProperties(scValue, data)
  of skAdditionalProperties:
    echo "Handle additional properties"
    echo "Additional properties MUST NOT be handled individually!"
  of skPatternProperties:
    #echo "Handle pattern properties"
    var matchedKeys = initHashSet[string]()
    let optRes = handlePatternProperties(scValue, data, matchedKeys)
    if optRes.isSome:
      result = get(optRes)
    else:
      result = true#false
  of skAllOf:
    #echo "Handle allOf "
    result = handleAllOf(scValue, data)
  of skAnyOf:
    #echo "Handle anyOf "
    result = handleAnyOf(scValue, data)
  of skOneOf:
    #echo "Handle oneOf "
    result = handleOneOf(scValue, data)
  of skIf:
    #echo "Handle if "
    result = handleIfThenElse(scValue, data)
  of skThen:
    echo "Handle then "
    echo "Then MUST NOT be handled individually!"
  of skElse:
    echo "Handle else "
    echo "Else MUST NOT be handled individually!"
  of skPropertyNames:
    #echo "Handle property names"
    result = handlePropertyNames(scValue, data)
  of skDependency:
    #echo "Handle dependencies"
    result = handleDependency(scValue, data)
  of skRef:
    echo "Handle ref"
    echo "Ref MUST NOT be handled individually!"

  #else:
  #  echo "Invalid for ", scKind

proc validateImpl*(schema: JsonNode, data: JsonNode): bool =
  result = true
  #echo "validating something ", schema, " TYPE ", schema.kind
  case schema.kind
  of JObject:
    for k, v in schema:
      # check each element of the schema on the data
      var scKind: SchemaKind
      try:
        scKind = parseEnum[SchemaKind](k)
      except ValueError:
        # in this case field is *probably* just a look field to be used
        # in the `CurrentSchema` via some `ref`
        continue
      var addItems: JsonNode
      var res: bool
      case scKind
      of skItems:
        # check if additional items exists
        let addItems = schema{$skAdditionalItems}
        # build object containing only `items` and `additionalItems`
        let scObj = %* {$skItems: v, $skAdditionalItems: addItems}
        res = handleKind(scKind, scObj, data)
      of skAdditionalItems:
        # skip, because dealt with together with items
        continue
      of skProperties, skAdditionalProperties:
        let scObj = preparePropObj(schema)
        res = handleKind(skProperties, scObj, data)
      of skIf, skThen, skElse:
        let scObj = prepareIfThenElseObj(schema)
        res = handleKind(skIf, scObj, data)
      of skRef:
        # found a reference, resolve it using whole schema
        let refNode = resolveReference(schema, v.getStr)
        #var resSchema = origSchema
        #resSchema.delete(k)
        #resSchema[refNode] = refNode
        #echo "Schema to be validated: ", refNode, " of ", schema
        res = validateImpl(refNode[1], data)
        # is reference, so skip rest of this `schema`
        return result and res
      #of skAdditionalProperties:
      #  # skip, because dealt with together with properties
      #  continue
      else:
        # deal with current scKind
        res = handleKind(scKind, v, data)

      if not res:
        #echo "Handled kind ", scKind, " for v ", v, " in data ", data
        result = false
        break
      #echo "Handled kind successful ", scKind, " for v ", v, " in data ", data
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

proc validate*(schema: JsonNode, data: JsonNode): bool =
  CurrentSchema = schema
  result = validateImpl(schema, data)
