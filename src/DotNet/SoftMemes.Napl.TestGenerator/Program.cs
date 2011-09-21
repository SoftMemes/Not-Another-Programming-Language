using System;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using ProtoBuf;
using SoftMemes.Napl.Serialization.TestRecords;

namespace SoftMemes.Napl.TestGenerator
{
    public static class Program
    {
        public static int Main(string[] args)
        {
            if (args.Length != 1)
            {
                Console.WriteLine("Usage: TestGenerator <outputdir>");
                return 1;
            }

            var outputDir = args[0];

            foreach (var t in TestData.TestDatas)
            {
                var filename = GetTestRecordFilename(outputDir, t);

                using (var naplStream = File.Open(filename, FileMode.Create))
                {
                    var testRecord = new NaplTestRecord();
                    testRecord.expression = NaplSerializer.SerializeExpression(t.Expression);
                    testRecord.expression_type = NaplSerializer.SerializeType(t.ExpectedExpressionType);
                    testRecord.test_cases.AddRange(t.TestCases.Select(SerializeTestCase));
                    testRecord.category = t.Category;
                    testRecord.description = t.Description;
                    Serializer.Serialize(naplStream, testRecord);
                }
            }

            return 0;
        }

        private static NaplTestCase SerializeTestCase(TestCase testCase)
        {
            var res = new NaplTestCase();
            res.arguments.AddRange(testCase.Arguments.Select(SerializeTestValue));
            res.expected_result = SerializeTestValue(testCase.ExpectedResult);
            res.description = testCase.Description;
            return res;
        }

        private static NaplTestValue SerializeTestValue(object value)
        {
            if (value is bool)
            {
                return new NaplTestValue
                {
                    value_type = NaplTestValueType.BooleanTestValueType,
                    boolean_value = (bool)value,
                };
            }
            else if (value is int)
            {
                return new NaplTestValue
                {
                    value_type = NaplTestValueType.IntegerTestValueType,
                    integer_value = (int)value,
                };
            }
            else if (value is double)
            {
                return new NaplTestValue
                {
                    value_type = NaplTestValueType.FloatTestValueType,
                    float_value = (double)value,
                };
            }
            else if (value is string)
            {
                return new NaplTestValue
                {
                    value_type = NaplTestValueType.StringTestValueType,
                    string_value = (string)value,
                };
            }
            else if (value.GetType().IsGenericType
                     && value.GetType().GetGenericTypeDefinition() == typeof(NaplExpression<>))
            {
                var serializeMethod =
                    typeof(NaplSerializer)
                    .GetMethod("SerializeExpression")
                    .MakeGenericMethod(value.GetType().GetGenericArguments()[0]);

                return new NaplTestValue
                {
                    value_type = NaplTestValueType.FunctionTestValueType,
                    expression_value = (Serialization.NaplExpression)serializeMethod.Invoke(null, new[]{value}),
                };
            }
            else
            {
                // TODO: Tuples
                throw new ArgumentException();
            }
        }

        private static string GetTestRecordFilename(string baseOutputDir, TestRecord t)
        {
            var categoryDir = Path.Combine(baseOutputDir, SanitizeFilename(t.Category));
            Directory.CreateDirectory(categoryDir);

            var filename = SanitizeFilename(t.Description) + ".napltest";
            return Path.Combine(categoryDir, filename);
        }

        private static string SanitizeFilename(string name)
        {
            string invalidChars = Regex.Escape(new string(Path.GetInvalidFileNameChars()));
            string invalidReStr = string.Format(@"[{0}]+", invalidChars);
            return Regex.Replace(name, invalidReStr, "_");
        }
    }
}
