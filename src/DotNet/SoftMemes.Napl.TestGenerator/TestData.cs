using System;
using System.Linq;

namespace SoftMemes.Napl.TestGenerator
{
    internal sealed class TestCase
    {
        private object _expectedResult;
        private readonly object[] _arguments;

        public TestCase(object expectedResult, params object[] arguments)
        {
            _expectedResult = expectedResult;
            _arguments = arguments;
        }

        public object ExpectedResult
        {
            get { return _expectedResult; }
            set { _expectedResult = value; }
        }

        public object[] Arguments
        {
            get { return _arguments; }
        }
    }

    internal sealed class TestRecord
    {
        private readonly string _category;
        private readonly string _description;
        private readonly NaplExpression<Unit> _expression;
        private readonly NaplType _expectedExpressionType;
        private readonly TestCase[] _testCases;

        public TestRecord(
            string category,
            string description,
            NaplExpression<Unit> expression,
            NaplType expectedExpressionType,
            TestCase[] testCases)
        {
            _category = category;
            _description = description;
            _expression = expression;
            _expectedExpressionType = expectedExpressionType;
            _testCases = testCases;
        }

        public string Category
        {
            get { return _category; }
        }

        public string Description
        {
            get { return _description; }
        }

        public NaplExpression<Unit> Expression
        {
            get { return _expression; }
        }

        public NaplType ExpectedExpressionType
        {
            get { return _expectedExpressionType; }  
        } 

        public TestCase[] TestCases
        {
            get { return _testCases; }
        }
    }

    internal static class TestData
    {
        public static readonly TestRecord[] TestDatas = new[]
        {
            simpleBoolCase("simple values", "boolean false", false),
            simpleBoolCase("simple values", "boolean true", true),
            simpleIntCase("simple values", "integer 0", 0),
            simpleIntCase("simple values", "integer min", int.MinValue),
            simpleIntCase("simple values", "integer max", int.MaxValue),
            simpleFloatCase("simple values", "float 0", 0.0),
            simpleFloatCase("simple values", "float min", double.MinValue),
            simpleFloatCase("simple values", "float max", double.MaxValue),
            simpleStringCase("simple values", "empty string", ""),
            simpleStringCase("simple values", "string", "Hello world"),
            simpleStringCase("simple values", "string with nulls", "Hello world\0Hello again."),
            simpleStringCase("simple values", "string with unicode", "你好，你的世界如何使用Unicode？"),
        };

        private static TestRecord simpleBoolCase(string category, string description, bool data)
        {
            return new TestRecord(
                category,
                description,
                NaplExpressionBuilder.BooleanValue(data),
                NaplType.BooleanType,
                new[] { new TestCase(data) });
        }

        private static TestRecord simpleIntCase(string category, string description, int data)
        {
            return new TestRecord(
                category,
                description,
                NaplExpressionBuilder.IntegerValue(data),
                NaplType.IntegerType,
                new[] { new TestCase(data) });
        }

        private static TestRecord simpleFloatCase(string category, string description, double data)
        {
            return new TestRecord(
                category,
                description,
                NaplExpressionBuilder.FloatValue(data),
                NaplType.FloatType,
                new[] { new TestCase(data) });
        }

        private static TestRecord simpleStringCase(string category, string description, string data)
        {
            return new TestRecord(
                category,
                description,
                NaplExpressionBuilder.StringValue(data),
                NaplType.StringType,
                new[] { new TestCase(data) });
        }
    }
}
