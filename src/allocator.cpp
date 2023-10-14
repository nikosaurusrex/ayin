#include "allocator.h"

#include <assert.h>

#ifdef _WIN64

#include <Windows.h>

byte *os_alloc_no_commit(u64 size) {
	return (byte *) VirtualAlloc(0, size, MEM_RESERVE, PAGE_NOACCESS);
}

void os_alloc_commit(byte *ptr, u64 size) {
	VirtualAlloc(ptr, size, MEM_COMMIT, PAGE_READWRITE);
}

#else

#include <sys/mman.h>

byte *os_alloc_no_commit(u64 size) {
	return (byte *) mmap(0, size, PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
}

void os_alloc_commit(byte *ptr, u64 size) {
	mprotect(ptr, size, PROT_READ | PROT_WRITE);
}

#endif

byte *libc_alloc(byte *context, u64 size) {
	return (byte *) malloc(size);
}

void libc_free(byte *context, byte *ptr) {
	free(ptr);
}

Allocator libc_allocator() {
	Allocator allocator = {};

	allocator.alloc_f = libc_alloc;
	allocator.free_f = libc_free;

	return allocator;
};

#include <stdio.h>

byte *arena_alloc(byte *context, u64 size) {
	Arena *arena = (Arena *) context;

	u64 end = arena->index + size;
	assert(end <= arena->size);
	
	byte *ptr = arena->base + arena->index;
	os_alloc_commit(ptr, size);

	arena->index = end;
	
	return ptr;
}

void arena_free(byte *context, byte *ptr) {
}

Allocator arena_allocator(Arena *arena) {
	Allocator allocator = {};

	allocator.context = (byte *) arena;
	allocator.alloc_f = arena_alloc;
	allocator.free_f = arena_free;

	assert(arena->size);
	
	arena->base = os_alloc_no_commit(arena->size);
	arena->index = 0;
	
	assert(arena->base);

	return allocator;
}

Allocator subarena_allocator(Allocator arena_allocator, Arena *subarena) {
	assert(arena_allocator.context);

	Arena *arena = (Arena *) arena_allocator.context;

	Allocator allocator = {};

	allocator.context = (byte *) subarena;
	allocator.alloc_f = arena_alloc;
	allocator.free_f = arena_free;

	assert(subarena->size);
	assert(arena->index + subarena->size <= arena->size);
	
	subarena->base = arena->base + arena->index;
	subarena->index = 0;

	arena->index += subarena->size;
	
	assert(subarena->base);

	return allocator;
}

uintptr_t align(uintptr_t ptr, uintptr_t align) {
	uintptr_t aligned = (ptr + (align - 1)) & (~(align - 1));
	return aligned;
}

byte *alloc(Allocator allocator, u64 size) {
	return allocator.alloc_f(allocator.context, size);
}

void free(Allocator allocator, byte *ptr) {
	allocator.free_f(allocator.context, ptr);
}
