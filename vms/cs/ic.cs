using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace Ic
{
    class Vm
    {
        Vm()
        {
            mem.AddRange(Enumerable.Repeat(0, 64));
        }

        List<int> mem = new();

        int ip = 0;
        int rb = 0;

        void ResizeMem(int addr)
        {
            if (addr >= mem.Count)
            {
                int oldCount = mem.Count;
                int newCount = oldCount;
                while (addr >= newCount) newCount <<= 1;
                mem.AddRange(Enumerable.Repeat(0, newCount - oldCount));
            }
        }

        int GetMem(int addr)
        {
            ResizeMem(addr);
            return mem[addr];
        }

        void SetMem(int addr, int val)
        {
            ResizeMem(addr);
            mem[addr] = val;
        }

        static readonly int[] MODE_MUL = { 100, 1000, 10000 };

        int GetParam(int idx)
        {
            int mode = GetMem(ip) / MODE_MUL[idx] % 10;
            switch (mode) {
                case 0: // position mode
                    return GetMem(GetMem(ip + idx + 1));
                case 1: // immediate mode
                    return GetMem(ip + idx + 1);
                case 2: // relative mode
                    return GetMem(rb + GetMem(ip + idx + 1));
                default:
                    throw new Exception(String.Format("mode error: ip {0} idx {1}", ip, idx));
            }
        }

        void SetParam(int idx, int val)
        {
            int mode = GetMem(ip) / MODE_MUL[idx] % 10;
            switch (mode) {
                case 0: // position mode
                    SetMem(GetMem(ip + idx + 1), val);
                    break;
                case 2: // relative mode
                    SetMem(rb + GetMem(ip + idx + 1), val);
                    break;
                default:
                    throw new Exception(String.Format("mode error: ip {0} idx {1}", ip, idx));
            }
        }

        public void Run(Func<int> getInput, Action<int> setOutput)
        {
            while (true) {
                int oc = GetMem(ip) % 100;

                switch (oc) {
                    case 1: // add
                        SetParam(2, GetParam(0) + GetParam(1));
                        ip += 4;
                        break;
                    case 2: // mul
                        SetParam(2, GetParam(0) * GetParam(1));
                        ip += 4;
                        break;
                    case 3: { // in
                        int value = getInput();
                        SetParam(0, value);
                        ip += 2;
                        break;
                    }
                    case 4: { // out
                        int value = GetParam(0);
                        ip += 2;
                        SetOutput(value);
                        break;
                    }
                    case 5: // jnz
                        if (GetParam(0) != 0) {
                            ip = GetParam(1);
                        } else {
                            ip += 3;
                        }
                        break;
                    case 6: // jz
                        if (GetParam(0) == 0) {
                            ip = GetParam(1);
                        } else {
                            ip += 3;
                        }
                        break;
                    case 7: // lt
                        SetParam(2, GetParam(0) < GetParam(1) ? 1 : 0);
                        ip += 4;
                        break;
                    case 8: // eq
                        SetParam(2, GetParam(0) == GetParam(1) ? 1 : 0);
                        ip += 4;
                        break;
                    case 9: // arb
                        rb += GetParam(0);
                        ip += 2;
                        break;
                    case 99: // hlt
                        return;
                    default:
                        throw new Exception(String.Format("opcode error: ip {0} oc {1}", ip, oc));
                }
            }
        }

        static int GetInput()
        {
            var buffer = new char [1];
            if (Console.In.ReadBlock(buffer, 0, 1) == 0)
            {
                throw new Exception("no more inputs");
            }
            return buffer[0];
        }

        static void SetOutput(int val)
        {
            Console.Write(Convert.ToChar(val));
            Console.Out.Flush();
        }

        static void Main(string[] args)
        {
            var data = File.ReadAllText(args[0]);

            Vm vm = new Vm();

            var idx = 0;
            foreach (var item in data.Trim().Split(','))
            {
                vm.SetMem(idx++, Convert.ToInt32(item));
            }

            vm.Run(GetInput, SetOutput);
        }
    }
}
