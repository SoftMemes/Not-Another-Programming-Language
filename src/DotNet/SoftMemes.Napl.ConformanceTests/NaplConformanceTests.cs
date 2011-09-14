using System.Collections.Generic;
using NUnit.Framework;
using SoftMemes.Napl.Serialization;
using SoftMemes.Napl.Serialization.TestRecords;
using System;

namespace SoftMemes.Napl.ConformanceTests
{
    [TestFixture]
    internal sealed class NaplConformanceTests
    {
        [Test, TestCaseSource(typeof(NUnitTestLoader), "ReadTestCases")]
        public void Test_Napl_test_case(
            Serialization.NaplExpression expression,
            Serialization.NaplType expressionType,
            List<NaplTestValue> arguments,
            NaplTestValue expectedResult)
        {
            var naplExpression = NaplSerializer.DeserializeExpression(expression);
            var naplExpressionType = NaplSerializer.DeserializeType(expressionType);

            var expressionClrType = NaplCompiler.GetClrType(naplExpressionType);
            var linqExpression = NaplCompiler.Compile(naplExpression);

            dynamic linqExpressionDyn = linqExpression;
            var linqExpressionDel = (Delegate)linqExpressionDyn.Compile();
            var res = linqExpressionDel.DynamicInvoke(arguments.ToArray());

            // TODO: Verify type checking
            // TODO: Verify serialization/deserialization
            Assert.AreEqual(GetClrValue(expectedResult), res);
        }

        private object GetClrValue(NaplTestValue naplValue)
        {
            switch (naplValue.value_type)
            {
                case NaplTestValueType.BooleanTestValueType:
                    return naplValue.boolean_value;
                case NaplTestValueType.IntegerTestValueType:
                    return naplValue.integer_value;
                case NaplTestValueType.FloatTestValueType:
                    return naplValue.float_value;
                case NaplTestValueType.StringTestValueType:
                    return naplValue.string_value;
                case NaplTestValueType.TupleTestValueType:
                    throw new NotSupportedException();
                default:
                    throw new ArgumentException();
            }
        }
    }
}
