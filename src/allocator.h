#ifndef ALLOCATOR_H
#define ALLOCATOR_H

#include "types.h"

const u64 KILO_BYTE = 1024;
const u64 MEGA_BYTE = KILO_BYTE * KILO_BYTE;
const u64 GIGA_BYTE = MEGA_BYTE * KILO_BYTE;

typedef u8 byte;

struct Allocator {
	byte *(*alloc_f)(byte *context, u64 size);
	void (*free_f)(byte *context, byte *ptr);
	byte *context;
};

// Set size before calling arena_allocator
struct Arena {
	byte *base = 0;
	u64 size = 0;
	u64 index = 0;
};

Allocator libc_allocator();
Allocator arena_allocator(Arena *arena);
Allocator subarena_allocator(Allocator allocator, Arena *subarena);

u8 *align(u8 *ptr, u8 align);

byte *alloc(Allocator allocator, u64 size);
void free(Allocator allocator, byte *ptr);

#endif
