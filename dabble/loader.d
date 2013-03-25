/*
 * Memory DLL loading code
 * Version 0.0.2
 *
 * Copyright (c) 2004-2005 by Joachim Bauch / mail@joachim-bauch.de
 * http://www.joachim-bauch.de
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is MemoryModule.c
 *
 * The Initial Developer of the Original Code is Joachim Bauch.
 *
 * Portions created by Joachim Bauch are Copyright (C) 2004-2005
 * Joachim Bauch. All Rights Reserved.
 *
 */

// disable warnings about pointer <. DWORD conversions
//#pragma warning( disable : 4311 4312 )

module dabble.loader;

import
    std.c.string,
    std.c.stdlib,
    core.sys.windows.windows;

extern(C) int    stricmp(char* s1, char* s2);
extern(Windows)
{
    BOOL HeapFree(HANDLE, DWORD, LPVOID);
    BOOL FreeLibrary(HINSTANCE);
    LPVOID HeapAlloc(HANDLE, DWORD, DWORD);
    HANDLE GetProcessHeap();
    HINSTANCE LoadLibraryA(LPCSTR);
    FARPROC GetProcAddress(HINSTANCE, LPCSTR);
    LPVOID VirtualAlloc(LPVOID, DWORD, DWORD, DWORD);
    BOOL VirtualFree(LPVOID, DWORD, DWORD);
    BOOL VirtualProtect(LPVOID, DWORD, DWORD, PDWORD);
    BOOL IsBadReadPtr(const VOID*, UINT_PTR);
}

const IMAGE_SIZEOF_BASE_RELOCATION=         8;
const IMAGE_REL_BASED_ABSOLUTE=             0;
const IMAGE_REL_BASED_HIGHLOW=              3;
const IMAGE_DIRECTORY_ENTRY_EXPORT=          0;   // Export Directory
const IMAGE_DIRECTORY_ENTRY_IMPORT=          1;   // Import Directory
const IMAGE_DIRECTORY_ENTRY_RESOURCE=        2;   // Resource Directory
const IMAGE_DIRECTORY_ENTRY_EXCEPTION=       3;   // Exception Directory
const IMAGE_DIRECTORY_ENTRY_SECURITY=        4;   // Security Directory
const IMAGE_DIRECTORY_ENTRY_BASERELOC=       5;   // Base Relocation Table
const IMAGE_DIRECTORY_ENTRY_DEBUG=           6;   // Debug Directory
const IMAGE_DIRECTORY_ENTRY_ARCHITECTURE=    7;   // Architecture Specific Data
const IMAGE_DIRECTORY_ENTRY_GLOBALPTR=       8;   // RVA of GP
const IMAGE_DIRECTORY_ENTRY_TLS=             9;   // TLS Directory
const IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG=    10;   // Load Configuration Directory
const IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT=   11;   // Bound Import Directory in headers
const IMAGE_DIRECTORY_ENTRY_IAT=            12;   // Import Address Table
const IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT=   13;   // Delay Load Import Descriptors
const IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR= 14;   // COM Runtime descriptor
const IMAGE_SCN_LNK_NRELOC_OVFL=            0x01000000;  // Section contains extended relocations.
const IMAGE_SCN_MEM_DISCARDABLE=            0x02000000;  // Section can be discarded.
const IMAGE_SCN_MEM_NOT_CACHED=             0x04000000;  // Section is not cachable.
const IMAGE_SCN_MEM_NOT_PAGED=              0x08000000;  // Section is not pageable.
const IMAGE_SCN_MEM_SHARED=                 0x10000000;  // Section is shareable.
const IMAGE_SCN_MEM_EXECUTE=                0x20000000;  // Section is executable.
const IMAGE_SCN_MEM_READ=                   0x40000000;  // Section is readable.
const IMAGE_SCN_MEM_WRITE=                  0x80000000;  // Section is writeable.
const IMAGE_SCN_CNT_INITIALIZED_DATA=       0x00000040;  // Section contains initialized data.
const IMAGE_SCN_CNT_UNINITIALIZED_DATA=     0x00000080;  // Section contains uninitialized data.

const IMAGE_ORDINAL_FLAG = 0x80000000;
bool IMAGE_SNAP_BY_ORDINAL(DWORD o) {return (o&IMAGE_ORDINAL_FLAG)!=0; }
DWORD IMAGE_ORDINAL32(DWORD Ordinal) { return Ordinal & 0xffff; }
alias IMAGE_ORDINAL32 IMAGE_ORDINAL;

alias void* HMEMORYMODULE;

struct IMAGE_DOS_HEADER  	  // DOS .EXE header
{
    WORD   e_magic;					 // Magic number
    WORD   e_cblp;					  // Bytes on last page of file
    WORD   e_cp;						// Pages in file
    WORD   e_crlc;					  // Relocations
    WORD   e_cparhdr;				   // Size of header in paragraphs
    WORD   e_minalloc;				  // Minimum extra paragraphs needed
    WORD   e_maxalloc;				  // Maximum extra paragraphs needed
    WORD   e_ss;						// Initial (relative) SS value
    WORD   e_sp;						// Initial SP value
    WORD   e_csum;					  // Checksum
    WORD   e_ip;						// Initial IP value
    WORD   e_cs;						// Initial (relative) CS value
    WORD   e_lfarlc;					// File address of relocation table
    WORD   e_ovno;					  // Overlay number
    WORD   e_res[4];					// Reserved words
    WORD   e_oemid;					 // OEM identifier (for e_oeminfo)
    WORD   e_oeminfo;				   // OEM information; e_oemid specific
    WORD   e_res2[10];				  // Reserved words
    LONG   e_lfanew;					// File address of new exe header
}

alias IMAGE_DOS_HEADER* PIMAGE_DOS_HEADER;

struct IMAGE_IMPORT_BY_NAME
{
    WORD    Hint;
    BYTE    Name[1];
}

alias IMAGE_IMPORT_BY_NAME* PIMAGE_IMPORT_BY_NAME;

struct IMAGE_NT_HEADERS
{
    DWORD Signature;
    IMAGE_FILE_HEADER FileHeader;
    IMAGE_OPTIONAL_HEADER32 OptionalHeader;
}

alias IMAGE_NT_HEADERS* PIMAGE_NT_HEADERS;

//LONG FIELD_OFFSET(type,field){ return cast(LONG)cast(LONG_PTR)&((cast(type*)null).field); }

PIMAGE_SECTION_HEADER IMAGE_FIRST_SECTION( PIMAGE_NT_HEADERS ntheader )
{
    return cast(PIMAGE_SECTION_HEADER)(
               cast(DWORD)ntheader +
               cast(LONG)cast(LONG_PTR)&((cast(IMAGE_NT_HEADERS*)null).OptionalHeader) +
               (cast(PIMAGE_NT_HEADERS)(ntheader)).FileHeader.SizeOfOptionalHeader
           );
}

struct IMAGE_FILE_HEADER
{
    WORD	Machine;
    WORD	NumberOfSections;
    DWORD   TimeDateStamp;
    DWORD   PointerToSymbolTable;
    DWORD   NumberOfSymbols;
    WORD	SizeOfOptionalHeader;
    WORD	Characteristics;
}

alias IMAGE_FILE_HEADER* PIMAGE_FILE_HEADER;

struct IMAGE_DATA_DIRECTORY
{
    DWORD VirtualAddress;
    DWORD Size;
}

alias IMAGE_DATA_DIRECTORY* PIMAGE_DATA_DIRECTORY;

const IMAGE_NUMBEROF_DIRECTORY_ENTRIES = 16;
const IMAGE_SIZEOF_SHORT_NAME = 8;

struct IMAGE_OPTIONAL_HEADER32
{
    //
    // Standard fields.
    //

    WORD	Magic;
    BYTE	MajorLinkerVersion;
    BYTE	MinorLinkerVersion;
    DWORD   SizeOfCode;
    DWORD   SizeOfInitializedData;
    DWORD   SizeOfUninitializedData;
    DWORD   AddressOfEntryPoint;
    DWORD   BaseOfCode;
    DWORD   BaseOfData;

    //
    // NT additional fields.
    //

    DWORD   ImageBase;
    DWORD   SectionAlignment;
    DWORD   FileAlignment;
    WORD	MajorOperatingSystemVersion;
    WORD	MinorOperatingSystemVersion;
    WORD	MajorImageVersion;
    WORD	MinorImageVersion;
    WORD	MajorSubsystemVersion;
    WORD	MinorSubsystemVersion;
    DWORD   Win32VersionValue;
    DWORD   SizeOfImage;
    DWORD   SizeOfHeaders;
    DWORD   CheckSum;
    WORD	Subsystem;
    WORD	DllCharacteristics;
    DWORD   SizeOfStackReserve;
    DWORD   SizeOfStackCommit;
    DWORD   SizeOfHeapReserve;
    DWORD   SizeOfHeapCommit;
    DWORD   LoaderFlags;
    DWORD   NumberOfRvaAndSizes;
    IMAGE_DATA_DIRECTORY DataDirectory[IMAGE_NUMBEROF_DIRECTORY_ENTRIES];
}

alias IMAGE_OPTIONAL_HEADER32* PIMAGE_OPTIONAL_HEADER32;

struct IMAGE_SECTION_HEADER
{
    BYTE	Name[IMAGE_SIZEOF_SHORT_NAME];
    union _Misc
    {
        DWORD   PhysicalAddress;
        DWORD   VirtualSize;
    }
    _Misc Misc;
    DWORD   VirtualAddress;
    DWORD   SizeOfRawData;
    DWORD   PointerToRawData;
    DWORD   PointerToRelocations;
    DWORD   PointerToLinenumbers;
    WORD	NumberOfRelocations;
    WORD	NumberOfLinenumbers;
    DWORD   Characteristics;
}

alias IMAGE_SECTION_HEADER* PIMAGE_SECTION_HEADER;

struct IMAGE_BASE_RELOCATION
{
    DWORD   VirtualAddress;
    DWORD   SizeOfBlock;
}

alias IMAGE_BASE_RELOCATION* PIMAGE_BASE_RELOCATION;

struct IMAGE_IMPORT_DESCRIPTOR
{
    union
    {
        DWORD   Characteristics;			// 0 for terminating null import descriptor
        DWORD   OriginalFirstThunk;		 // RVA to original unbound IAT (PIMAGE_THUNK_DATA)
    }
    DWORD   TimeDateStamp;				  // 0 if not bound,
    // -1 if bound, and real date\time stamp
    //	 in IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT (new BIND)
    // O.W. date/time stamp of DLL bound to (Old BIND)

    DWORD   ForwarderChain;				 // -1 if no forwarders
    DWORD   Name;
    DWORD   FirstThunk;					 // RVA to IAT (if bound this IAT has actual addresses)
}

alias IMAGE_IMPORT_DESCRIPTOR* PIMAGE_IMPORT_DESCRIPTOR;

struct IMAGE_EXPORT_DIRECTORY
{
    DWORD   Characteristics;
    DWORD   TimeDateStamp;
    WORD	MajorVersion;
    WORD	MinorVersion;
    DWORD   Name;
    DWORD   Base;
    DWORD   NumberOfFunctions;
    DWORD   NumberOfNames;
    DWORD   AddressOfFunctions;	 // RVA from base of image
    DWORD   AddressOfNames;		 // RVA from base of image
    DWORD   AddressOfNameOrdinals;  // RVA from base of image
}

alias IMAGE_EXPORT_DIRECTORY* PIMAGE_EXPORT_DIRECTORY;

struct MEMORYMODULE
{
    PIMAGE_NT_HEADERS headers;
    ubyte *codeBase;
    HMODULE *modules;
    int numModules;
    int initialized;
}

alias MEMORYMODULE* PMEMORYMODULE;

extern(Windows) alias BOOL function(HINSTANCE hinstDLL, DWORD fdwReason, LPVOID lpReserved) DllEntryProc;

IMAGE_DATA_DIRECTORY* GET_HEADER_DICTIONARY(MEMORYMODULE* mod,int idx) {return &mod.headers.OptionalHeader.DataDirectory[idx];}

version(DEBUG_OUTPUT)
{
    void OutputLastError(char *msg)
    {
        LPVOID tmp;
        char *tmpmsg;
        FormatMessageA(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                      null, GetLastError(), MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), cast(LPTSTR)&tmp, 0, null);
        tmpmsg = cast(char *)LocalAlloc(LPTR, strlen(msg) + strlen(tmp) + 3);
        sprintf(tmpmsg, "%s: %s", msg, tmp);
        OutputDebugString(tmpmsg);
        LocalFree(tmpmsg);
        LocalFree(tmp);
    }
}

void* getSectionBase(HMEMORYMODULE mod, string nameBegins /* name begins with this string */)
in
{
    assert(nameBegins.length < IMAGE_SIZEOF_SHORT_NAME);
}
body
{
    auto pmod = cast(PMEMORYMODULE)mod;
    PIMAGE_SECTION_HEADER section = IMAGE_FIRST_SECTION(pmod.headers);
    for (int i = 0; i < pmod.headers.FileHeader.NumberOfSections; i++, section++)
    {
        if (section.Name[0..nameBegins.length] == nameBegins)
            return pmod.codeBase + section.VirtualAddress;
    }
    return null;
}


void getImageBounds(HMEMORYMODULE mod, out void* start, out void* stop)
{
    auto pmod = cast(PMEMORYMODULE)mod;
    PIMAGE_SECTION_HEADER section = IMAGE_FIRST_SECTION(pmod.headers);
    size_t size = 0;
    start = pmod.codeBase + section.VirtualAddress;
    for (int i = 0; i < pmod.headers.FileHeader.NumberOfSections; i++, section++)
        size += section.SizeOfRawData;
    stop = start + size;
}

void CopySections(ubyte *data, PIMAGE_NT_HEADERS old_headers, PMEMORYMODULE modul)
{
    int i, size;
    ubyte *codeBase = modul.codeBase;
    ubyte *dest;
    PIMAGE_SECTION_HEADER section = IMAGE_FIRST_SECTION(modul.headers);
    for (i = 0; i < modul.headers.FileHeader.NumberOfSections; i++, section++)
    {
        if (section.SizeOfRawData == 0)
        {
            // section doesn't contain data in the dll itself, but may define
            // uninitialized data
            size = old_headers.OptionalHeader.SectionAlignment;
            if (size > 0)
            {
                dest = cast(ubyte *)VirtualAlloc(codeBase + section.VirtualAddress,
                                                 size,
                                                 MEM_COMMIT,
                                                 PAGE_READWRITE);

                section.Misc.PhysicalAddress = cast(DWORD)dest;
                memset(dest, 0, size);
            }

            // section is empty
            continue;
        }

        // commit memory block and copy data from dll
        dest = cast(ubyte *)VirtualAlloc(codeBase + section.VirtualAddress,
                                         section.SizeOfRawData,
                                         MEM_COMMIT,
                                         PAGE_READWRITE);
        memcpy(dest, data + section.PointerToRawData, section.SizeOfRawData);
        section.Misc.PhysicalAddress = cast(DWORD)dest;
    }
}

// Protection flags for memory pages (Executable, Readable, Writeable)
static int ProtectionFlags[2][2][2] =
[
    [ // not executable
        [PAGE_NOACCESS, PAGE_WRITECOPY],
        [PAGE_READONLY, PAGE_READWRITE],
    ],
    [ // executable
        [PAGE_EXECUTE, PAGE_EXECUTE_WRITECOPY],
        [PAGE_EXECUTE_READ, PAGE_EXECUTE_READWRITE]
    ]
];

void FinalizeSections(PMEMORYMODULE modul)
{
    int i;
    PIMAGE_SECTION_HEADER section = IMAGE_FIRST_SECTION(modul.headers);

    // loop through all sections and change access flags
    for (i=0; i<modul.headers.FileHeader.NumberOfSections; i++, section++)
    {
        DWORD protect, oldProtect, size;
        int executable = (section.Characteristics & IMAGE_SCN_MEM_EXECUTE) != 0;
        int readable =   (section.Characteristics & IMAGE_SCN_MEM_READ) != 0;
        int writeable =  (section.Characteristics & IMAGE_SCN_MEM_WRITE) != 0;

        if ((section.Characteristics & IMAGE_SCN_MEM_DISCARDABLE) && !executable)
        {
            // section is not needed any more and can safely be freed
            VirtualFree(cast(LPVOID)section.Misc.PhysicalAddress, section.SizeOfRawData, MEM_DECOMMIT);
            continue;
        }

        // determine protection flags based on characteristics
        protect = ProtectionFlags[executable][readable][writeable];
        if (section.Characteristics & IMAGE_SCN_MEM_NOT_CACHED)
            protect |= PAGE_NOCACHE;

        // determine size of region
        size = section.SizeOfRawData;
        if (size == 0)
        {
            if (section.Characteristics & IMAGE_SCN_CNT_INITIALIZED_DATA)
                size = modul.headers.OptionalHeader.SizeOfInitializedData;
            else if (section.Characteristics & IMAGE_SCN_CNT_UNINITIALIZED_DATA)
                size = modul.headers.OptionalHeader.SizeOfUninitializedData;
        }

        if (size > 0)
        {
            // change memory access flags
            if (VirtualProtect(cast(LPVOID)section.Misc.PhysicalAddress, section.SizeOfRawData, protect, &oldProtect) == 0)
            {
                version(DEBUG_OUTPUT) OutputLastError("Error protecting memory page");
            }
        }
    }
}

void PerformBaseRelocation(PMEMORYMODULE modul, DWORD delta)
{
    import std.stdio;

    DWORD i;
    ubyte *codeBase = modul.codeBase;

    PIMAGE_DATA_DIRECTORY directory = GET_HEADER_DICTIONARY(modul, IMAGE_DIRECTORY_ENTRY_BASERELOC);
    if (directory.Size > 0)
    {
        PIMAGE_BASE_RELOCATION relocation = cast(PIMAGE_BASE_RELOCATION)(codeBase + directory.VirtualAddress);
        for (; relocation.VirtualAddress > 0; )
        {
            ubyte *dest = cast(ubyte *)(codeBase + relocation.VirtualAddress);
            ushort *relInfo = cast(ushort *)(cast(ubyte *)relocation + IMAGE_SIZEOF_BASE_RELOCATION);
            for (i=0; i<((relocation.SizeOfBlock-IMAGE_SIZEOF_BASE_RELOCATION) / 2); i++, relInfo++)
            {
                DWORD *patchAddrHL;
                int type, offset;

                // the upper 4 bits define the type of relocation
                type = *relInfo >> 12;
                // the lower 12 bits define the offset
                offset = *relInfo & 0xfff;

                switch (type)
                {
                    case IMAGE_REL_BASED_ABSOLUTE:
                        // skip relocation
                        break;

                    case IMAGE_REL_BASED_HIGHLOW:
                        // change complete 32 bit address
                        patchAddrHL = cast(DWORD *)(dest + offset);
                        *patchAddrHL += delta;
                        break;

                    default:
                        //printf("Unknown relocation: %d\n", type);
                        break;
                }
            }

            // advance to next relocation block
            relocation = cast(PIMAGE_BASE_RELOCATION)((cast(DWORD)relocation) + relocation.SizeOfBlock);
        }
    }
}

int BuildImportTable(PMEMORYMODULE modul)
{
    int result=1;
    ubyte *codeBase = modul.codeBase;

    PIMAGE_DATA_DIRECTORY directory = GET_HEADER_DICTIONARY(modul, IMAGE_DIRECTORY_ENTRY_IMPORT);
    if (directory.Size > 0)
    {
        PIMAGE_IMPORT_DESCRIPTOR importDesc = cast(PIMAGE_IMPORT_DESCRIPTOR)(codeBase + directory.VirtualAddress);
        for (; !IsBadReadPtr(importDesc, IMAGE_IMPORT_DESCRIPTOR.sizeof) && importDesc.Name; importDesc++)
        {
            DWORD* thunkRef,funcRef;
            HMODULE handle = LoadLibraryA(cast(LPCSTR)(codeBase + importDesc.Name));
            if (handle == INVALID_HANDLE_VALUE)
            {
                version(DEBUG_OUTPUT) OutputLastError("Can't load library");
                result = 0;
                break;
            }

            modul.modules = cast(HMODULE *)realloc(modul.modules, (modul.numModules+1)*(HMODULE.sizeof));
            if (modul.modules == null)
            {
                result = 0;
                break;
            }

            modul.modules[modul.numModules++] = handle;
            if (importDesc.OriginalFirstThunk)
            {
                thunkRef = cast(DWORD *)(codeBase + importDesc.OriginalFirstThunk);
                funcRef = cast(DWORD *)(codeBase + importDesc.FirstThunk);
            }
            else
            {
                // no hint table
                thunkRef = cast(DWORD *)(codeBase + importDesc.FirstThunk);
                funcRef = cast(DWORD *)(codeBase + importDesc.FirstThunk);
            }
            for (; *thunkRef; thunkRef++, funcRef++)
            {
                if(IMAGE_SNAP_BY_ORDINAL(*thunkRef))
                    *funcRef = cast(DWORD)GetProcAddress(handle, cast(LPCSTR)IMAGE_ORDINAL(*thunkRef));
                else
                {
                    PIMAGE_IMPORT_BY_NAME thunkData = cast(PIMAGE_IMPORT_BY_NAME)(codeBase + *thunkRef);
                    *funcRef = cast(DWORD)GetProcAddress(handle, cast(LPCSTR)&thunkData.Name);
                }
                if (*funcRef == 0)
                {
                    result = 0;
                    break;
                }
            }

            if (!result)
                break;
        }
    }

    return result;
}

HMEMORYMODULE MemoryLoadLibrary(void *data)
{
    enum
    {
        IMAGE_DOS_SIGNATURE = 0x5A4D,
        IMAGE_NT_SIGNATURE = 0x00004550
    }

    PMEMORYMODULE result;
    PIMAGE_DOS_HEADER dos_header;
    PIMAGE_NT_HEADERS old_header;
    ubyte *code,headers;
    DWORD locationDelta;
    DllEntryProc DllEntry;
    BOOL successfull;

    dos_header = cast(PIMAGE_DOS_HEADER)data;
    if (dos_header.e_magic != IMAGE_DOS_SIGNATURE)
    {
        version(DEBUG_OUTPUT) OutputDebugString("Not a valid executable file.\n");
        return null;
    }

    old_header = cast(PIMAGE_NT_HEADERS)&(cast(ubyte *)(data))[dos_header.e_lfanew];
    if (old_header.Signature != IMAGE_NT_SIGNATURE)
    {
        version(DEBUG_OUTPUT) OutputDebugString("No PE header found.\n");
        return null;
    }

    // reserve memory for image of library
    code = cast(ubyte *)VirtualAlloc(cast(LPVOID)(old_header.OptionalHeader.ImageBase),
                                     old_header.OptionalHeader.SizeOfImage,
                                     MEM_RESERVE,
                                     PAGE_READWRITE);

    if (code == null)
        // try to allocate memory at arbitrary position
        code = cast(ubyte *)VirtualAlloc(null,
                                         old_header.OptionalHeader.SizeOfImage,
                                         MEM_RESERVE,
                                         PAGE_READWRITE);

    if (code == null)
    {
        version(DEBUG_OUTPUT) OutputLastError("Can't reserve memory");
        return null;
    }

    result = cast(PMEMORYMODULE)HeapAlloc(GetProcessHeap(), 0, MEMORYMODULE.sizeof);
    result.codeBase = code;
    result.numModules = 0;
    result.modules = null;
    result.initialized = 0;

    // XXX: is it correct to commit the complete memory region at once?
    //	  calling DllEntry raises an exception if we don't...
    VirtualAlloc(code,
                 old_header.OptionalHeader.SizeOfImage,
                 MEM_COMMIT,
                 PAGE_READWRITE);

    // commit memory for headers
    headers = cast(ubyte *)VirtualAlloc(code,
                                        old_header.OptionalHeader.SizeOfHeaders,
                                        MEM_COMMIT,
                                        PAGE_READWRITE);

    // copy PE header to code
    memcpy(headers, dos_header, dos_header.e_lfanew + old_header.OptionalHeader.SizeOfHeaders);
    result.headers = cast(PIMAGE_NT_HEADERS)&(cast(ubyte *)(headers))[dos_header.e_lfanew];

    // update position
    result.headers.OptionalHeader.ImageBase = cast(DWORD)code;

    // copy sections from DLL file block to new memory location
    CopySections(cast(ubyte*)data, old_header, result);

    // adjust base address of imported data
    locationDelta = cast(DWORD)(code - old_header.OptionalHeader.ImageBase);
    if (locationDelta != 0)
        PerformBaseRelocation(result, locationDelta);

    // load required dlls and adjust function table of imports
    if (!BuildImportTable(result))
        goto error;

    // mark memory pages depending on section headers and release
    // sections that are marked as "discardable"
    FinalizeSections(result);

    // get entry point of loaded library
    if (result.headers.OptionalHeader.AddressOfEntryPoint != 0)
    {
        DllEntry = cast(DllEntryProc)cast(void*)(code + result.headers.OptionalHeader.AddressOfEntryPoint);
        if (DllEntry is null)
        {
            version(DEBUG_OUTPUT) OutputDebugString("Library has no entry point.\n");
            goto error;
        }

        // notify library about attaching to process
        successfull = (*DllEntry)(cast(HINSTANCE)code, DLL_PROCESS_ATTACH, null);
        if (!successfull)
        {
            version(DEBUG_OUTPUT) OutputDebugString("Can't attach library.\n");
            goto error;
        }
        result.initialized = 1;
    }

    return cast(HMEMORYMODULE)result;

error:
    // cleanup
    MemoryFreeLibrary(result);
    return null;
}

FARPROC MemoryGetProcAddress(HMEMORYMODULE modul, char *name)
{
    ubyte *codeBase = (cast(PMEMORYMODULE)modul).codeBase;
    int idx=-1;
    DWORD i;
    DWORD* nameRef;
    WORD *ordinal;
    PIMAGE_EXPORT_DIRECTORY exports;
    PIMAGE_DATA_DIRECTORY directory = GET_HEADER_DICTIONARY(cast(PMEMORYMODULE)modul, IMAGE_DIRECTORY_ENTRY_EXPORT);
    if (directory.Size == 0)
        // no export table found
        return null;

    exports = cast(PIMAGE_EXPORT_DIRECTORY)(codeBase + directory.VirtualAddress);
    if (exports.NumberOfNames == 0 || exports.NumberOfFunctions == 0)
        // DLL doesn't export anything
        return null;

    // search function name in list of exported names
    nameRef = cast(DWORD *)(codeBase + exports.AddressOfNames);
    ordinal = cast(WORD *)(codeBase + exports.AddressOfNameOrdinals);
    for (i=0; i<exports.NumberOfNames; i++, nameRef++, ordinal++)
        if (stricmp(name, cast(char *)(codeBase + *nameRef)) == 0)
        {
            idx = *ordinal;
            break;
        }

    if (idx == -1)
        // exported symbol not found
        return null;

    if (cast(DWORD)idx > exports.NumberOfFunctions)
        // name <. ordinal number don't match
        return null;

    // AddressOfFunctions contains the RVAs to the "real" functions
    return cast(FARPROC)(codeBase + *cast(DWORD *)(codeBase + exports.AddressOfFunctions + (idx*4)));
}

void MemoryFreeLibrary(HMEMORYMODULE mod, bool callDetach = true)
{
    int i;
    PMEMORYMODULE modul = cast(PMEMORYMODULE)mod;

    if (modul != null)
    {
        if (modul.initialized != 0)
        {
            // notify library about detaching from process
            if (callDetach)
            {
                DllEntryProc DllEntry = cast(DllEntryProc)(modul.codeBase + modul.headers.OptionalHeader.AddressOfEntryPoint);
                (*DllEntry)(cast(HINSTANCE)modul.codeBase, DLL_PROCESS_DETACH, null);
            }
            modul.initialized = 0;
        }

        if (modul.modules != null)
        {
            // free previously opened libraries
            for (i=0; i<modul.numModules; i++)
                if (modul.modules[i] != INVALID_HANDLE_VALUE)
                    FreeLibrary(modul.modules[i]);

            free(modul.modules);
        }

        if (modul.codeBase != null)
            // release memory of library
            VirtualFree(modul.codeBase, 0, MEM_RELEASE);

        HeapFree(GetProcessHeap(), 0, modul);
    }
}

