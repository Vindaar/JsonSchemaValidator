import unittest, json
import ../schemaValidator

#    {
#        "description": "not",
#        "schema": {
#            "not": {"type": "integer"}
#        },
#        "tests": [
#            {
#                "description": "allowed",
#                "data": "foo",
#                "valid": true
#            },
#            {
#                "description": "disallowed",
#                "data": 1,
#                "valid": false
#            }
#        ]
#    },
#

proc validateTest(test: JsonNode): bool =
  echo test
  let testName = test["description"].getStr
  let schema = test["schema"]

  result = true
  for t in test["tests"]:
    # get data to validate
    let data = t["data"]
    let testRes = validate(schema, data)
    let expectedResult = t["valid"].getBool
    echo "This test ", testRes, " aa ", data, " for expected ", expectedResult
    echo "\n\n\n"
    result = result and (testRes == expectedResult)


proc validateTests(file: JsonNode): bool =
  # file is JArray of tests
  result = true
  for test in file:
    result = result and validateTest(test)

suite "Additional Items":
  test "Test1":
    let tests = parseFile("tests/additionalItems.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

suite "All Of":
  test "Test1":
    let tests = parseFile("tests/allOf.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

suite "Any Of":
  test "Test1":
    let tests = parseFile("tests/anyOf.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

suite "Constants":
  test "Test1":
    let tests = parseFile("tests/const.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

suite "Contains":
  test "Test1":
    let tests = parseFile("tests/contains.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

suite "Definitions":
  test "Test1":
    let tests = parseFile("tests/definitions.json")


suite "Enumerations":
  test "Test1":
    let tests = parseFile("tests/enum.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

suite "Exclusive Maximum":
  test "Test1":
    let tests = parseFile("tests/exclusiveMaximum.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

suite "Exclusive Minimum":
  test "Test1":
    let tests = parseFile("tests/exclusiveMinimum.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

suite "If then else":
  test "Test1":
    let tests = parseFile("tests/if-then-else.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

suite "Items":
  test "Test1":
    let tests = parseFile("tests/items.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

suite "maximum.json":
  test "Test1":
    let tests = parseFile("tests/maximum.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

suite "maxItems.json":
  test "Test1":
    let tests = parseFile("tests/maxItems.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

suite "maxLength.json":
  test "Test1":
    let tests = parseFile("tests/maxLength.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

suite "maxProperties.json":
  test "Test1":
    let tests = parseFile("tests/maxProperties.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

suite "minimum.json":
  test "Test1":
    let tests = parseFile("tests/minimum.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

suite "minItems.json":
  test "Test1":
    let tests = parseFile("tests/minItems.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

suite "minLength.json":
  test "Test1":
    let tests = parseFile("tests/minLength.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

suite "minProperties.json":
  test "Test1":
    let tests = parseFile("tests/minProperties.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

suite "multipleOf.json":
  test "Test1":
    let tests = parseFile("tests/multipleOf.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

suite "not.json":
  test "Test1":
    let tests = parseFile("tests/not.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

suite "oneOf.json":
  test "Test1":
    let tests = parseFile("tests/oneOf.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

#optional/
suite "pattern.json":
  test "Test1":
    let tests = parseFile("tests/pattern.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

suite "patternProperties.json":
  test "Test1":
    let tests = parseFile("tests/patternProperties.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

suite "properties.json":
  test "Test1":
    let tests = parseFile("tests/properties.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

suite "refRemote.json":
  test "Test1":
    let tests = parseFile("tests/refRemote.json")

suite "required.json":
  test "Test1":
    let tests = parseFile("tests/required.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

suite "type.json":
  test "Test1":
    let tests = parseFile("tests/type.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

suite "uniqueItems.json":
  test "Test1":
    let tests = parseFile("tests/uniqueItems.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

suite "Additional Properties":
  test "Test1":
    let tests = parseFile("tests/additionalProperties.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

suite "propertyNames.json":
  test "Test1":
    let tests = parseFile("tests/propertyNames.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

suite "Dependencies":
  test "Test1":
    let tests = parseFile("tests/dependencies.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res

suite "ref.json":
  test "Test1":
    let tests = parseFile("tests/ref.json")
    let res = validateTests(tests)
    echo "Res is ", res
    check res
