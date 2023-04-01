import Data.Xml.Token
import Data.Xml.Parse
import System.Exit
import Test.HUnit

testTokenize = TestCase
    (
        assertEqual
        "Wrong tokenization"
        (tokenize "<test>42</test>")
        [XmlOpenTagToken "test", XmlLiteralToken "42", XmlClosingTagToken "test"]
    )

testSimpleXml = TestCase
    (
        assertEqual
        "Wrong xml tree"
        (parseXml "<test>42</test>")
        (XmlTag "test" [] [XmlLiteral "42"])
    )

testAttributes = TestCase
    (
        assertEqual
        "Wrong attributes values"
        (parseXml "<test value=\"42\" place=\"ohio\" value=\"-1^3\"></test>")
        (XmlTag "test" [("value", "42"), ("place", "ohio"), ("value", "-1^3")] [])
    )

testNesting = TestCase
    (
        assertEqual
        "Wrong xml tree"
        (parseXml "       \n\
\<s1>                     \n\
\    <s2>                 \n\
\        <s3>value1</s3>  \n\
\    </s2>                \n\
\    <s4>value2</s4>      \n\
\</s1>                    \n\
\ ")
        (XmlTag "s1" [] [XmlTag "s2" [] [XmlTag "s3" [] [XmlLiteral "value1"]], XmlTag "s4" [] [XmlLiteral "value2"]])
    )

tests = TestList
    [
          TestLabel "tokenize" testTokenize
        , TestLabel "simple xml" testSimpleXml
        , TestLabel "attributes" testAttributes
        , TestLabel "nesting" testNesting
    ]

main = do
    results <- runTestTT tests
    if errors results + failures results == 0
    then exitSuccess
    else exitFailure
