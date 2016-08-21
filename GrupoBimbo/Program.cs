using Accord.Statistics.Models.Regression.Linear;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace GrupoBimbo
{
    class Program
    {
        static void Main(string[] args)
        {
            using (var db = new GrupoBimboEntities())
            {
                var tests = db.tests.Take(10);

                foreach (var test in tests)
                {
                    var orderEntities = db.trains.Where(o =>
                                                    o.Agencia_ID == test.Agencia_ID &&
                                                    o.Canal_ID == test.Canal_ID &&
                                                    o.Ruta_SAK == test.Ruta_SAK &&
                                                    o.Cliente_ID == test.Cliente_ID &&
                                                    o.Producto_ID == test.Producto_ID).ToList();

                    if (orderEntities.Count < 5)
                        continue;

                    var orders = new double[9][];

                    for (int i = 0; i < 9; i++)
                        orders[i] = new double[] { i, 0 };

                    foreach (var order in orderEntities)
                        orders[order.Semana-1][1] = order.Venta_uni_hoy - order.Dev_uni_proxima;

                    var regressionCalculator = new MultipleLinearRegression(2);
                    var regression = regressionCalculator.Transform(orders);

                    Debugger.Break();
                }
            }
        }

        static T[][] ListToMultidimensionalArray<T>(List<List<T>> list)
        {
            T[][] arr = new T[list.Count][];

            for (int i = 0; i < list.Count; i++)
                arr[i] = list[i].ToArray();

            return arr;
        }
    }
}