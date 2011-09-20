using SoftMemes.Napl.Linq;

namespace SoftMemes.Napl.TestGenerator
{
    internal sealed class TestCase
    {
        private readonly string _description;
        private readonly object _expectedResult;
        private readonly object[] _arguments;

        public TestCase(
            string description,
            object expectedResult,
            params object[] arguments)
        {
            _description = description;
            _expectedResult = expectedResult;
            _arguments = arguments;
        }

        public string Description
        {
            get { return _description; }
        }

        public object ExpectedResult
        {
            get { return _expectedResult; }
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
            // Simple values ...
            SimpleBoolRecord("simple values", "boolean false", false),
            SimpleBoolRecord("simple values", "boolean true", true),
            SimpleIntRecord("simple values", "integer 0", 0),
            SimpleIntRecord("simple values", "integer min", int.MinValue),
            SimpleIntRecord("simple values", "integer max", int.MaxValue),
            SimpleFloatRecord("simple values", "float 0", 0.0),
            SimpleFloatRecord("simple values", "float min", double.MinValue),
            SimpleFloatRecord("simple values", "float max", double.MaxValue),
            SimpleStringRecord("simple values", "empty string", ""),
            SimpleStringRecord("simple values", "string", "Hello world"),
            SimpleStringRecord("simple values", "string with nulls", "Hello world\0Hello again."),
            SimpleStringRecord("simple values", "string with unicode", "你好，你的世界如何使用Unicode？"),

            // Operators
            FunctionRecord(
                "operators",
                "bool equality",
                Linq.Expr((bool x1, bool x2) => x1 == x2).ToNapl(),
                NaplTypeBuilder.Function(
                    NaplTypeBuilder.Boolean,
                    new[]{NaplTypeBuilder.Boolean, NaplTypeBuilder.Boolean}),
                new TestCase("false = false", true, false, false),
                new TestCase("false = true", false, false, true),
                new TestCase("true = false", false, true, false),
                new TestCase("true = true", true, true, true)),
            FunctionRecord(
                "operators",
                "int equality",
                Linq.Expr((int x1, int x2) => x1 == x2).ToNapl(),
                NaplTypeBuilder.Function(
                    NaplTypeBuilder.Boolean,
                    new[]{NaplTypeBuilder.Integer, NaplTypeBuilder.Integer}),
                new TestCase("0 = 0", true, 0, 0),
                new TestCase("0 = 1", false, 0, 1),
                new TestCase("min = min", true, int.MinValue, int.MinValue),
                new TestCase("max = max", true, int.MaxValue, int.MaxValue),
                new TestCase("-1 = 1", false, -1, 1)),
        };

        private static TestRecord SimpleBoolRecord(string category, string description, bool data)
        {
            return new TestRecord(
                category,
                description,
                NaplExpressionBuilder.BooleanValue(data),
                NaplType.BooleanType,
                new[] { new TestCase(description, data) });
        }

        private static TestRecord SimpleIntRecord(string category, string description, int data)
        {
            return new TestRecord(
                category,
                description,
                NaplExpressionBuilder.IntegerValue(data),
                NaplType.IntegerType,
                new[] { new TestCase(description, data) });
        }

        private static TestRecord SimpleFloatRecord(string category, string description, double data)
        {
            return new TestRecord(
                category,
                description,
                NaplExpressionBuilder.FloatValue(data),
                NaplType.FloatType,
                new[] { new TestCase(description, data) });
        }

        private static TestRecord SimpleStringRecord(string category, string description, string data)
        {
            return new TestRecord(
                category,
                description,
                NaplExpressionBuilder.StringValue(data),
                NaplType.StringType,
                new[] { new TestCase(description, data) });
        }

        private static TestRecord FunctionRecord<T>(
            string category,
            string recordDescription,
            NaplExpression<T> expression,
            NaplType expressionType,
            params TestCase[] cases)
        {
            return new TestRecord(
                category,
                recordDescription,
                NaplExpressionBuilder.Untag(expression),
                expressionType,
                cases);
        }
    }
}
