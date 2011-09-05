using System;
using System.Linq.Expressions;
using SoftMemes.Napl.Language;
using System.IO;

namespace SoftMemes.Napl.TestApp
{
    class Program
    {
        static void Main(string[] args)
        {
            var p1 = Parameter.NewParameter(NaplType.IntegerType, "p1");
            var p2 = Parameter.NewParameter(NaplType.IntegerType, "p2");

            var eif = Expression.If(
                Expression.BinaryOperator(
                    Operator.LessThanOperator,
                    Expression.Parameter(p1),
                    Expression.Parameter(p2)),
                Expression.StringValue("Yep!"),
                Expression.StringValue("Nope :("));

            var e = Expression.Lambda(new[]{p1,p2}, eif);

            Console.WriteLine(
                "Parsed expression: {0}",
                PrettyPrinter.PrintExpression(e));

            var et = Validator.TypeCheck(e);
            Console.WriteLine(
                "Type checked expression: {0}",
                PrettyPrinter.PrintExpression(et));

            var ec = Compiler.Compile(et);
            Console.WriteLine("Compiled expression: {0}", ec);

            while (ec.CanReduce) ec = ec.Reduce();            
            Console.WriteLine("Reduced expression: {0}", ec);

            var func = (Func<int, int, string>)((LambdaExpression)ec).Compile();
            Console.WriteLine("Result: {0}", func(42, 52));

            var serialization = Serializer.Serialize(et);
            Console.WriteLine("Serialization: {0}", serialization);

            var stream = new MemoryStream();
            ProtoBuf.Serializer.Serialize<Serialization.Expression>(stream, serialization);

            Console.ReadLine();
        }
    }
}
