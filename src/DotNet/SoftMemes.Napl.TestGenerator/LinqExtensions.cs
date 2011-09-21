using System;
using System.Linq.Expressions;

namespace SoftMemes.Napl.TestGenerator
{
    public static class Linq
    {
        public static Expression Expr<T1, TRet>(
            Expression<Func<T1, TRet>> expr)
        {
            return expr;
        }
       
        public static Expression Expr<T1, T2, TRet>(
             Expression<Func<T1, T2, TRet>> expr)
         {
             return expr;
         }
    }
}