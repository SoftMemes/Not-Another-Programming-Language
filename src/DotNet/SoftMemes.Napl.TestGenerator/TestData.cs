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

    internal sealed class TestExpression
    {
        private readonly string _category;
        private readonly string _description;
        private readonly NaplExpression<Unit> _expression;
        private readonly Type[] _expectedSignature;
        private readonly TestCase[] _testCases;

        public TestExpression(
            string category,
            string description,
            NaplExpression<Unit> expression,
            Type[] expectedSignature,
            TestCase[] testCases)
        {
            _category = category;
            _description = description;
            _expression = expression;
            _expectedSignature = expectedSignature;
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

        public Type[] ExpectedSignature
        {
            get { return _expectedSignature; }  
        } 

        public TestCase[] TestCases
        {
            get { return _testCases; }
        }
    }

    internal static class TestData
    {
        public static readonly TestExpression[] TestDatas = new[]
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

        private static TestExpression simpleBoolCase(string category, string description, bool data)
        {
            return FromExpression<bool>(
                category,
                description,
                NaplExpressionBuilder.BooleanValue(data),
                Tuple.Create(data));
        }

        private static TestExpression simpleIntCase(string category, string description, int data)
        {
            return FromExpression<int>(
                category,
                description,
                NaplExpressionBuilder.IntegerValue(data),
                Tuple.Create(data));
        }

        private static TestExpression simpleFloatCase(string category, string description, double data)
        {
            return FromExpression<double>(
                category,
                description,
                NaplExpressionBuilder.FloatValue(data),
                Tuple.Create(data));
        }

        private static TestExpression simpleStringCase(string category, string description, string data)
        {
            return FromExpression<string>(
                category,
                description,
                NaplExpressionBuilder.StringValue(data),
                Tuple.Create(data));
        }
        
        private static TestExpression FromExpression<TOut>(
            string category,
            string description,
            NaplExpression<Unit> expr,
            params Tuple<TOut>[] cases)
        {
            return new TestExpression(
                category,
                description,
                expr,
                new[] { typeof(TOut) },
                cases.Select(e => new TestCase(e.Item1)).ToArray());
        }
    }
}
