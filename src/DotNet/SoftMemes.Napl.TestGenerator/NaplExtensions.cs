using System;

namespace SoftMemes.Napl.TestGenerator
{
    internal static class NaplExtensions
    {
        public static NaplExpression<Unit> ToNaplExpression(this object value)
        {
            if (value is bool)
            {
                return NaplExpressionBuilder.BooleanValue((bool)value);
            }
            else if (value is int)
            {
                return NaplExpressionBuilder.IntegerValue((int)value);
            }
            else if (value is double)
            {
                return NaplExpressionBuilder.FloatValue((double)value);
            }
            else if (value is string)
            {
                return NaplExpressionBuilder.StringValue((string)value);
            }
            else
            {
                throw new ArgumentException();
            }
        }

        public static NaplType ToNaplType(this Type type)
        {
            if (type == typeof(bool))
            {
                return NaplType.BooleanType;
            }
            else if (type == typeof(int))
            {
                return NaplType.IntegerType;
            }
            else if (type == typeof(double))
            {
                return NaplType.FloatType;
            }
            else if (type == typeof(string))
            {
                return NaplType.StringType;
            }
            else
            {
                // TODO: Function types
                throw new ArgumentException();
            }
        }
    }
}
