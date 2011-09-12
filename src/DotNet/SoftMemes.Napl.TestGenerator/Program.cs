using System;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using ProtoBuf;

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
                var categoryDir = Path.Combine(outputDir, SanitizeFilename(t.Category));
                Directory.CreateDirectory(categoryDir);

                var filename = SanitizeFilename(t.Description);
                var naplFilename = Path.Combine(categoryDir, filename + ".napl");
                var testCasesFilename = Path.Combine(categoryDir, filename + ".txt");

                using (var naplStream = File.Open(naplFilename, FileMode.Create))
                {
                    Serializer.Serialize(naplStream, NaplSerializer.Serialize(t.Expression));
                }

                using (var testCasesStream = File.Open(testCasesFilename, FileMode.Create))
                using (var testCasesWriter = new StreamWriter(testCasesStream, Encoding.UTF8))
                {
                    testCasesWriter.WriteLine(
                        string.Join("\t", t.ExpectedSignature.Select(ToPortableType)));

                    foreach (var testCase in t.TestCases)
                    {
                        testCasesWriter.WriteLine(
                            string.Join(
                                "\t",
                                Enumerable
                                .Concat(testCase.Arguments, new[] { testCase.ExpectedResult })
                                .Select(ToPortableLiteral)));
                    }
                }
            }

            return 0;
        }

        private static string EscapeString(string str)
        {
            return str
                .Replace("\0", "\\0")
                .Replace("\t", "\\t")
                .Replace("\r", "\\r")
                .Replace("\n", "\\n");
        }

        private static string ToPortableLiteral(object o)
        {
            if (o is bool)
            {
                return (bool)o ? "true" : "false";
            }
            else if (o is string)
            {
                return EscapeString((string)o);
            }
            else if (o is double)
            {
                return ((double)o).ToString("r", CultureInfo.InvariantCulture);
            }
            else
            {
                return string.Format(CultureInfo.InvariantCulture, "{0}", o);
            }
        }

        private static string ToPortableType(Type type)
        {
            if (type == typeof(bool))
            {
                return "boolean";
            }
            else if (type == typeof(int))
            {
                return "int";
            }
            else if (type == typeof(double))
            {
                return "float";
            }
            else if (type == typeof(string))
            {
                return "string";
            }
            else 
            {
                throw new ArgumentException();
            }
        }

        private static string SanitizeFilename(string name)
        {
            string invalidChars = Regex.Escape(new string(Path.GetInvalidFileNameChars()));
            string invalidReStr = string.Format(@"[{0}]+", invalidChars);
            return Regex.Replace(name, invalidReStr, "_");
        }
    }
}
