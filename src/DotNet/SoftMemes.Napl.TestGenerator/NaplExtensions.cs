using System;

namespace SoftMemes.Napl.TestGenerator
{
    internal static class NaplExtensions
    {
        public static NaplExpression<Unit> ToNaplValue(this object value)
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
    }
}
