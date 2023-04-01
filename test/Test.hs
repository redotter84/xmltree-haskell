import System.Exit
import Test.HUnit

import Data.Xml.Parse
import Data.Xml.Token
import Data.Xml.Tree

xmlTreeSimple = "<test>42</test>"
xmlTreeNested = "    \n\
\<s1>                     \n\
\    <s2>                 \n\
\        <s3>value1</s3>  \n\
\    </s2>                \n\
\    <s4>value2</s4>      \n\
\</s1>                    \n\
\ "
xmlTreeWithAttributes = "<test value=\"42\" place=\"ohio\" value=\"-1^3\"></test>"

testTokenize = TestCase
    (
        assertEqual
        "Wrong tokenization"
        (tokenize xmlTreeSimple)
        [XmlOpenTagToken "test", XmlLiteralToken "42", XmlClosingTagToken "test"]
    )

testSimpleXml = TestCase
    (
        assertEqual
        "Wrong xml tree"
        (parseXml xmlTreeSimple)
        (XmlTag "test" [] [XmlLiteral "42"])
    )

testAttributes = TestCase
    (
        assertEqual
        "Wrong attributes values"
        (parseXml xmlTreeWithAttributes)
        (XmlTag "test" [XmlAttribute "value" "42", XmlAttribute "place" "ohio", XmlAttribute "value" "-1^3"] [])
    )

testNesting = TestCase
    (
        assertEqual
        "Wrong xml tree"
        (parseXml xmlTreeNested)
        (XmlTag "s1" [] [XmlTag "s2" [] [XmlTag "s3" [] [XmlLiteral "value1"]], XmlTag "s4" [] [XmlLiteral "value2"]])
    )

testFindChild = TestCase
    (
        assertEqual
        "Wrong child"
        ((findXmlTreeChild 1 . parseXml) xmlTreeNested)
        (XmlTag "s4" [] [XmlLiteral "value2"])
    )

testFindAttributes = TestCase
    (
        assertEqual
        "Wrong attributes values"
        ((findXmlTreeAttribute "value" . parseXml) xmlTreeWithAttributes)
        ["42", "-1^3"]
    )

tests = TestList
    [
          TestLabel "tokenize" testTokenize
        , TestLabel "simple_xml" testSimpleXml
        , TestLabel "attributes" testAttributes
        , TestLabel "nesting" testNesting
        , TestLabel "find_child" testFindChild
        , TestLabel "find_attributes" testFindAttributes
    ]

main = do
    results <- runTestTT tests
    if errors results + failures results == 0
    then exitSuccess
    else exitFailure
