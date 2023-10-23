#ifndef COMMON_H
#define COMMON_H

#include <assert.h>
#include <cstdarg>
#include <cstdio>
#include <stdlib.h>
#include <string.h>

#include "types.h"

#include "allocator.h"

struct String {
	char *data = 0;
	s64 length = 0;

	String() {
		data = 0;
	}

    String(const char *cstr) {
        data = (char *) cstr;
        length = (s32) strlen(cstr);
    }

	char operator[](s64 index) const {
		assert(index >= 0 && index < length);

		return data[index];
	}

	String substring(s64 start, s64 slen) {
        assert(start < length && start+slen <= length);
        
        String s;
        s.data = data + start;
        s.length = slen;
        return  s;
    }
};

inline String to_string(const char *c_string) {
    String s;
    s.data = (char *) c_string;
    s.length = strlen(c_string);
    return s;
}

inline char *to_c_string(String s) {
    auto length = s.length;
    
    char *mem = (char *)malloc(length + 1);
    memcpy(mem, s.data, length);
    mem[s.length] = 0;
    return mem;
}

inline bool operator==(const String &s, const String &t) {
    if (s.length != t.length) return false;
    if (s.data == 0 && t.data != 0) return false;
    if (t.data == 0 && s.data != 0) return false;
    if (s.data == 0 && t.data == 0) return true;
    
    for (s64 i = 0; i < s.length; ++i) {
        if (s[i] != t[i]) return false;
    }
    
    return true;
}

inline bool operator!=(const String &s, const String &t) {
    return !(s == t);
}

inline String copy_string(String s) {
    String out;
    out.length = s.length;
    
    auto length = s.length;
    if (s.data && s.length) {
        out.data = (char *)malloc(length);
        memcpy(out.data, s.data, length);
    }
    return out;
}

inline String basepath(String s) {
    while (s.length && (s[s.length-1] == '/' || s[s.length-1] == '\\')) {
        s.length--;
    }
    while (s.length) {
        if (s[s.length-1] == '/' || s[s.length-1] == '\\') return s;
        
        s.length--;
    }
    
    return s;
}

inline String basename(String s) {
    auto length = s.length;

    s64 skip = 0;
    while (length && (s[length-1] == '/' || s[length-1] == '\\')) {
        length--;
        skip++;
    }

    while (length) {
        if (s[length-1] == '/' || s[length-1] == '\\') {
            String out;
            out.data   = s.data + length;
            out.length = s.length - (length + skip); 
            return out;
        }
        
        length--;
    }
    
    return s;
}

inline void convert_to_back_slashes(char *c) {
    while (*c) {
        if (*c == '/') {
            *c = '\\';
        }
        
        ++c;
    }
}

inline void convert_to_forward_slashes(char *c) {
    while (*c) {
        if (*c == '\\') {
            *c = '/';
        }
        
        ++c;
    }
}

template<typename T>
struct Array {
    Allocator allocator = libc_allocator();
    T *data = 0;
    s64 length = 0;
    s64 allocated = 0;
    
    const int NEW_MEM_CHUNK_ELEMENT_COUNT =  16;
    
    Array(s64 reserve_amount = 0) {
        reserve(reserve_amount);
    }
    
    ~Array() {
        reset();
    }
    
    void reserve(s64 amount) {
        if (amount <= 0) amount = NEW_MEM_CHUNK_ELEMENT_COUNT;
        if (amount <= allocated) return;
        
        T *new_mem = (T *)alloc(allocator, amount * sizeof(T));
        
        if (data) {
            memcpy(new_mem, data, length * sizeof(T));
            free(allocator, (byte *) data);
        }
        
        data = new_mem;
        allocated = amount;
    }
    
    void resize(s64 amount) {
        reserve(amount);
        length = amount;
    }
    
    void add(T element) {
        if (length+1 >= allocated) reserve(allocated * 2);
        
        data[length] = element;
        length += 1;
    }
    
    T unordered_remove(s64 index) {
        assert(index >= 0 && index < length);
        assert(length);
        
        T last = pop();
        if (index < length) {
            (*this)[index] = last;
        }
        
        return last;
    }

    T ordered_remove(s64 index) {
        assert(index >= 0 && index < length);
        assert(length);

        T item = (*this)[index];
        memmove(data + index, data + index + 1, ((length - index) - 1) * sizeof(T));

        length--;
        return item;
    }
    
    T pop() {
        assert(length > 0);
        T result = data[length-1];
        length -= 1;
        return result;
    }
    
    void clear() {
        length = 0;
    }
    
    void reset() {
        length = 0;
        allocated = 0;
        
        if (data) free(allocator, (byte *) data);
        data = 0;
    }
    
    T &operator[] (s64 index) {
        assert(index >= 0 && index < length);
        return data[index];
    }
    
    T *begin() {
        return &data[0];
    }
    
    T *end() {
        return &data[length];
    }
};

struct StringBuilder {
    const int BUCKET_ALLOC_SIZE = 4096;
    
    struct Bucket {
        u8 *data = 0;
        s64 count     = 0;
        s64 allocated = 0;
    };
    
    Array<Bucket> buckets;
    
    StringBuilder() {
        make_bucket(BUCKET_ALLOC_SIZE);
    }
    
    ~StringBuilder() {
        for (auto &bucket : buckets) {
            if (bucket.data) free(bucket.data);
            bucket.data = 0;
        }
        
        buckets.reset();
    }
    
    void make_bucket(s64 amount) {
        Bucket b;
        b.data = (u8 *)malloc(amount);
        b.allocated = amount;
        b.count = 0;
        buckets.add(b);
    }
    
    void putchar(char c) {
        auto bucket = &buckets[buckets.length-1];
        
        if (bucket->count < bucket->allocated) {
            bucket->data[bucket->count] = c;
            bucket->count++;
        } else {
            make_bucket(BUCKET_ALLOC_SIZE);
            putchar(c);
        }
    }
    
    void append(String s) {
        for (s64 i = 0; i < s.length; ++i) {
            putchar(s[i]);
        }
    }
    
    void append(char *s) {
        String o;
        o.data = s;
        o.length = strlen(s);
        append(o);
    }
    
    void print_valist(char *c_fmt, va_list vl) {
        va_list vl_copy;
        va_copy(vl_copy, vl);
        
        auto bucket = &buckets[buckets.length-1];
        auto remaining = bucket->allocated - bucket->count;
        auto written = vsnprintf((char *)bucket->data + bucket->count, remaining, c_fmt, vl);
        
        if (written < 0) return; 
        
        if (written < remaining) {
            // success
            bucket->count += written;
            assert(bucket->count <= bucket->allocated);
        } else {
            u8 *data = (u8 *)malloc(written + 1);
            auto final = vsnprintf((char *)data, written+1, c_fmt, vl_copy);
            
            assert(final >= 0);
            assert(final < written + 1);
            
            Bucket b;
            b.data = data;
            b.count = final;
            b.allocated = written+1;
            buckets.add(b);
        }
    }
    
    void print(char *c_fmt, ...) {
        va_list vl;
        va_start(vl, c_fmt);
        print_valist(c_fmt, vl);
        va_end(vl);
    }
    
    String to_string() {
        s64 total_data = 0;
        for (Bucket &b : buckets) {
            total_data += b.count;
        }
        
        char *data = (char *)malloc(total_data);
        s64 cursor = 0;
        for (Bucket &b : buckets) {
            memcpy(data+cursor, b.data, b.count);
            cursor += b.count;
        }
        
        assert(cursor == total_data);
        
        String s;
        s.data = data;
        s.length = total_data;
        return s;
    }
};

template<typename A, typename B>
struct Tuple {
    A first;
    B second;
};

template<typename A, typename B>
Tuple<A, B> MakeTuple(A a, B b) {
    Tuple<A, B> t;
    t.first = a;
    t.second = b;
    return t;
}

struct Atom {
	String id;
	u32 hash;
};

struct Atom_Table {
	Array<Atom *> data;

	Atom *find_atom(String id) {
		u32 hash = hash_str(id);
        for (s64 i = 0; i < data.length; ++i) {
            auto it = data[i];
            if (it->hash == hash) {
                if (it->id == id) return it;
            }
        }
        
        return 0;
	}
    
    Atom *find_atom_hash(String id) {
        u32 hash = hash_str(id);
        for (s64 i = 0; i < data.length; ++i) {
            auto it = data[i];
            if (it->hash == hash) {
                return it;
            }
        }
        
        return 0;
    }

	u32 hash_str(String str) {
		u32 hash = 5381;

        for (s64 i = 0; i < str.length; ++i) {
        	s16 c = (s16)(s8)str[i];
            hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
		}

        return hash;
	}
};

inline bool read_entire_file(String file_path, String *result) {
    char *cpath = to_c_string(file_path);
    
    FILE *file = fopen(cpath, "rb");
    if (!file) {
        free(cpath);
        return false;
    }
    
    fseek(file, 0, SEEK_END);
    auto size = ftell(file);
    fseek(file, 0, SEEK_SET);
    
    char *mem = (char *)malloc(size);
    auto bytes_read = fread(mem, 1, size, file);
    if (bytes_read != (size_t)size) {
        fclose(file);
        free(mem);
        free(cpath);
        return false;
    }
    
    String s;
    s.data = mem;
    s.length = size;
    *result = s;
    free(cpath);
    return true;
}


#define CONCAT_INTERNAL(x,y) x##y
#define CONCAT(x,y) CONCAT_INTERNAL(x,y)

template<typename T>
struct ExitScope {
    T lambda;
    ExitScope(T lambda):lambda(lambda){}
    ~ExitScope(){lambda();}
  private:
    ExitScope& operator =(const ExitScope&);
};

class ExitScopeHelp {
  public:
    template<typename T>
        ExitScope<T> operator+(T t){ return t;}
};

#if _MSC_VER
#define defer const auto& CONCAT(defer__, __LINE__) = ExitScopeHelp() + [&]()
#else // __GNUC__ or __clang__
#define defer const auto& __attribute__((unused)) CONCAT(defer__, __LINE__) = ExitScopeHelp() + [&]()
#endif

#if _MSC_VER
#define debug_break() __debugbreak()
#else // __GNUC__ or __clang__
#define debug_break() __builtin_debugtrap()
#endif

#endif
