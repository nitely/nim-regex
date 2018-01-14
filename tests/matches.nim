import ../src/regex
import unittest

const EMAIL = re"^[_]*([a-z0-9]+(\.|_*)?)+@([a-z][a-z0-9-]+(\.|-*\.))+[a-z]{2,6}$"

suite "Complex Matches Test":
    test "Should match email":
        check("hello.guy@google.com.au".fullMatch(EMAIL).isMatch == true)
    test "Should not match email":
        check("hello@google".fullMatch(EMAIL).isMatch == false)