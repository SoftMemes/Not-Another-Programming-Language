using System;
using System.Collections.Generic;
using System.IO;
using NUnit.Framework;
using ProtoBuf;
using SoftMemes.Napl.Serialization.TestRecords;

namespace SoftMemes.Napl.ConformanceTests
{
    internal static class NUnitTestLoader
    {
        public static IEnumerable<TestCaseData> ReadTestCases()
        {
            var testFiles =
                Directory.EnumerateFiles(".", "*.napltest", SearchOption.AllDirectories);

            foreach (var testFile in testFiles)
            {
                using (var testFileStream = File.OpenRead(testFile))
                {
                    var naplTestRecord = Serializer.Deserialize<NaplTestRecord>(testFileStream);
                    foreach (var naplTestCase in naplTestRecord.test_cases)
                    {
                        yield return new TestCaseData(
                            naplTestRecord.expression,
                            naplTestRecord.expression_type,
                            naplTestCase.arguments,
                            naplTestCase.expected_result)
                            .SetCategory(naplTestRecord.category)
                            .SetDescription(naplTestRecord.description)
                            .SetName(string.Format("{0} - {1} - {2}",
                                naplTestRecord.category,
                                naplTestRecord.description,
                                naplTestCase.description));
                    }
                }
            }
        }
    }
}
