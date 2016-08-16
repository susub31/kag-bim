using System;
using System.Collections.Generic;
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
            var clients = new Dictionary<int, string>();
            var lines = File.ReadAllLines("cliente_tabla.csv").Skip(1);
            var sb = new StringBuilder("Cliente_ID,NombreCliente\r\n");

            foreach (var line in lines)
            {
                var data = line.Split(',');

                var id = int.Parse(data[0]);
                var name = data[1];

                while (name.Contains("  "))
                    name = name.Replace("  ", " ");

                if (!clients.ContainsKey(id))
                {
                    clients.Add(id, name);
                    continue;
                }
                else
                {
                    if (clients[id].ToLower() == "sin nombre")
                        clients[id] = name;
                }
               
            }

            //Dictionary<string, int> clientNameCounts = new Dictionary<string, int>();

            foreach (var kvp in clients)
            {
                sb.AppendLine($"{kvp.Key},{kvp.Value}");
            }

            //foreach (var kvp in clientNameCounts)
            //{
            //    if (kvp.Value > 1)
            //    {
            //        sb.AppendLine($"Name: {kvp.Key} Count: {kvp.Value}");
            //    }
            //}

            File.WriteAllText("output.txt", sb.ToString());
        }
    }
}
