// -*- js2-basic-offset: 4 -*-
class Char {
    constructor(codepoint) {
        this.codepoint = codepoint;
    }
    toString() {
        let ch = String.fromCodePoint(this.codepoint);
        if (ch == '\n') return '#\\newline';
        if (ch == '\r') return '#\\return';
        if (ch.match(/[a-zA-Z0-9$[\]().]/)) return `#\\${ch}`;
        return `#\\x${this.codepoint.toString(16)}`;
    }
}
class Eof { toString() { return "#<eof>"; } }
class Nil { toString() { return "#nil"; } }
class Null { toString() { return "()"; } }
class Unspecified { toString() { return "#<unspecified>"; } }

class Complex {
    constructor(real, imag) {
        this.real = real;
        this.imag = imag;
    }
    toString() {
        const sign = this.imag >= 0 && Number.isFinite(this.imag) ? "+": "";
        return `${flonum_to_string(this.real)}${sign}${flonum_to_string(this.imag)}i`;
    }
}
class Fraction {
    constructor(num, denom) {
        this.num = num;
        this.denom = denom;
    }
    toString() {
        return `${this.num}/${this.denom}`;
    }
}

class HeapObject {
    constructor(reflector, obj) {
        this.reflector = reflector;
        this.obj = obj;
    }
    repr() { return this.toString(); } // Default implementation.
}

class Pair extends HeapObject {
    toString() { return "#<pair>"; }
    repr() {
        let car_repr = repr(this.reflector.car(this));
        let cdr_repr = repr(this.reflector.cdr(this));
        if (cdr_repr == '()')
            return `(${car_repr})`;
        if (cdr_repr.charAt(0) == '(')
            return `(${car_repr} ${cdr_repr.substring(1)}`;
        return `(${car_repr} . ${cdr_repr})`;
    }
}
class MutablePair extends Pair { toString() { return "#<mutable-pair>"; } }

class Vector extends HeapObject {
    toString() { return "#<vector>"; }
    repr() {
        let len = this.reflector.vector_length(this);
        let out = '#(';
        for (let i = 0; i < len; i++) {
            if (i) out += ' ';
            out += repr(this.reflector.vector_ref(this, i));
        }
        out += ')';
        return out;
    }
}
class MutableVector extends Vector {
    toString() { return "#<mutable-vector>"; }
}

class Bytevector extends HeapObject {
    toString() { return "#<bytevector>"; }
    repr() {
        let len = this.reflector.bytevector_length(this);
        let out = '#vu8(';
        for (let i = 0; i < len; i++) {
            if (i) out += ' ';
            out += this.reflector.bytevector_ref(this, i);
        }
        out += ')';
        return out;
    }
}
class MutableBytevector extends Bytevector {
    toString() { return "#<mutable-bytevector>"; }
}

class Bitvector extends HeapObject {
    toString() { return "#<bitvector>"; }
    repr() {
        let len = this.reflector.bitvector_length(this);
        let out = '#*';
        for (let i = 0; i < len; i++) {
            out += this.reflector.bitvector_ref(this, i) ? '1' : '0';
        }
        return out;
    }
}
class MutableBitvector extends Bitvector {
    toString() { return "#<mutable-bitvector>"; }
}

class MutableString extends HeapObject {
    toString() { return "#<mutable-string>"; }
    repr() { return string_repr(this.reflector.string_value(this)); }
}

class Procedure extends HeapObject {
    toString() { return "#<procedure>"; }
    call(...arg) {
        return this.reflector.call(this, ...arg);
    }
    async call_async(...arg) {
        return await this.reflector.call_async(this, ...arg);
    }
}

class Sym extends HeapObject {
    toString() { return "#<symbol>"; }
    repr() { return this.reflector.symbol_name(this); }
}

class Keyword extends HeapObject {
    toString() { return "#<keyword>"; }
    repr() { return `#:${this.reflector.keyword_name(this)}`; }
}

class Variable extends HeapObject { toString() { return "#<variable>"; } }
class AtomicBox extends HeapObject { toString() { return "#<atomic-box>"; } }
class HashTable extends HeapObject { toString() { return "#<hash-table>"; } }
class WeakTable extends HeapObject { toString() { return "#<weak-table>"; } }
class Fluid extends HeapObject { toString() { return "#<fluid>"; } }
class DynamicState extends HeapObject { toString() { return "#<dynamic-state>"; } }
class Syntax extends HeapObject { toString() { return "#<syntax>"; } }
class SyntaxTransformer extends HeapObject { toString() { return "#<syntax-transformer>"; } }
class Port extends HeapObject { toString() { return "#<port>"; } }
class Struct extends HeapObject { toString() { return "#<struct>"; } }

function instantiate_streaming(path, imports) {
    if (typeof fetch !== 'undefined' && typeof window !== 'undefined')
        return WebAssembly.instantiateStreaming(fetch(path), imports);
    let bytes;
    if (typeof read !== 'undefined') {
        bytes = read(path, 'binary');
    } else if (typeof readFile !== 'undefined') {
        bytes = readFile(path);
    } else {
        let fs = require('fs');
        bytes = fs.readFileSync(path);
    }
    return WebAssembly.instantiate(bytes, imports);
}

class IterableWeakSet {
    #array;
    #set;
    constructor() { this.#array = []; this.#set = new WeakSet; }
    #kill(i) {
        let tail = this.#array.pop();
        if (i < this.#array.length)
            this.#array[i] = tail;
    }
    #get(i) {
        if (i >= this.#array.length)
            return null;
        let obj = this.#array[i].deref();
        if (obj)
            return obj;
        this.#kill(i);
        return null;
    }
    #cleanup() {
        let i = 0;
        while (this.#get(i)) i++;
    }
    has(x) { return this.#set.has(x); }
    add(x) {
        if (this.has(x))
            return;
        if (this.#array.length % 32 == 0)
            this.#cleanup();
        this.#set.add(x);
        this.#array.push(new WeakRef(x));
    }
    delete(x) {
        if (!this.has(x))
            return;
        this.#set.delete(x);
        let i = 0;
        while (this.#get(i) != x) i++;
        this.#kill(i);
    }
    *[Symbol.iterator]() {
        for (let i = 0, x; x = this.#get(i); i++)
            yield x;
    }
}

class Scheme {
    #instance;
    #abi;
    constructor(instance, abi) {
        this.#instance = instance;
        this.#abi = abi;
    }

    static async reflect(abi, {reflect_wasm_dir = '.'}) {
        let debug = {
            debug_str(x) { console.log(`reflect debug: ${x}`); },
            debug_str_i32(x, y) { console.log(`reflect debug: ${x}: ${y}`); },
            debug_str_scm: (x, y) => {
                console.log(`reflect debug: ${x}: #<scm>`);
            },
            code_source(x) { return ['???', 0, 0]; }
        };
        let reflect_wasm = reflect_wasm_dir + '/reflect.wasm';
        let rt = {
            quit(status) { throw new SchemeQuitError(status); },
            die(tag, data) { throw new SchemeTrapError(tag, data); },
            wtf8_to_string(wtf8) { return wtf8_to_string(wtf8); },
            string_to_wtf8(str) { return string_to_wtf8(str); },
        };
        let { module, instance } =
            await instantiate_streaming(reflect_wasm, { abi, debug, rt });
        return new Scheme(instance, abi);
    }

    #init_module(mod) {
        mod.set_debug_handler({
            debug_str(x) { console.log(`debug: ${x}`); },
            debug_str_i32(x, y) { console.log(`debug: ${x}: ${y}`); },
            debug_str_scm: (x, y) => {
                console.log(`debug: ${x}: ${repr(this.#to_js(y))}`);
            },
        });
        mod.set_ffi_handler({
            is_extern_func: (x) => typeof(x) === "function",
            call_extern: (f, args) => {
                const jsArgs = [];
                for(args = this.#to_js(args); !(args instanceof Null); args = this.cdr(args)) {
                    jsArgs.push(this.car(args));
                }
                let result = f(...jsArgs);
                if(result === undefined || result === null) {
                    result = new Unspecified();
                }
                return this.#to_scm(result);
            },
            procedure_to_extern: (obj) => {
                const proc = this.#to_js(obj);
                return (...args) => {
                    return proc.call(...args)[0];
                };
            }
        });
        mod.set_finalization_handler({
            make_finalization_registry: (f) => new FinalizationRegistry(f),
            finalization_registry_register: (registry, target, heldValue) => {
                // heldValue is a Wasm struct and needs to be wrapped
                // so that when it goes back to Scheme via the
                // finalizer callback it is seen as a Scheme value and
                // not an external one.
                registry.register(target, this.#to_js(heldValue));
            },
            finalization_registry_register_with_token: (registry, target, heldValue, unregisterToken) => {
                registry.register(target, this.#to_js(heldValue), unregisterToken);
            },
            finalization_registry_unregister: (registry, unregisterToken) => {
                return registry.unregister(unregisterToken);
            }
        });
        let proc = new Procedure(this, mod.get_export('$load').value);
        return proc.call();
    }
    static async load_main(path, opts = {}) {
        let mod = await SchemeModule.fetch_and_instantiate(path, opts);
        let reflect = await mod.reflect(opts);
        return reflect.#init_module(mod);
    }
    async load_extension(path, opts = {}) {
        opts = Object.assign({ abi: this.#abi }, opts);
        let mod = await SchemeModule.fetch_and_instantiate(path, opts);
        return this.#init_module(mod);
    }

    #to_scm(js) {
        let api = this.#instance.exports;
        if (typeof(js) == 'number') {
            return api.scm_from_f64(js);
        } else if (typeof(js) == 'bigint') {
            if (BigInt(api.scm_most_negative_fixnum()) <= js
                && js <= BigInt(api.scm_most_positive_fixnum()))
                return api.scm_from_fixnum(Number(js));
            return api.scm_from_bignum(js);
        } else if (typeof(js) == 'boolean') {
            return js ? api.scm_true() : api.scm_false();
        } else if (typeof(js) == 'string') {
            return api.scm_from_string(js);
        } else if (typeof(js) == 'object') {
            if (js instanceof Eof) return api.scm_eof();
            if (js instanceof Nil) return api.scm_nil();
            if (js instanceof Null) return api.scm_null();
            if (js instanceof Unspecified) return api.scm_unspecified();
            if (js instanceof Char) return api.scm_from_char(js.codepoint);
            if (js instanceof HeapObject) return js.obj;
            if (js instanceof Fraction)
                return api.scm_from_fraction(this.#to_scm(js.num),
                                             this.#to_scm(js.denom));
            if (js instanceof Complex)
                return api.scm_from_complex(js.real, js.imag);
            return api.scm_from_extern(js);
        } else if (typeof(js) == 'function') {
            return api.scm_from_extern(js);
        } else {
            throw new Error(`unexpected; ${typeof(js)}`);
        }
    }

    #to_js(scm) {
        let api = this.#instance.exports;
        let descr = api.describe(scm);
        let handlers = {
            fixnum: () => BigInt(api.fixnum_value(scm)),
            char: () => new Char(api.char_value(scm)),
            true: () => true,
            false: () => false,
            eof: () => new Eof,
            nil: () => new Nil,
            null: () => new Null,
            unspecified: () => new Unspecified,
            flonum: () => api.flonum_value(scm),
            bignum: () => api.bignum_value(scm),
            complex: () => new Complex(api.complex_real(scm),
                                       api.complex_imag(scm)),
            fraction: () => new Fraction(this.#to_js(api.fraction_num(scm)),
                                         this.#to_js(api.fraction_denom(scm))),
            pair: () => new Pair(this, scm),
            'mutable-pair': () => new MutablePair(this, scm),
            vector: () => new Vector(this, scm),
            'mutable-vector': () => new MutableVector(this, scm),
            bytevector: () => new Bytevector(this, scm),
            'mutable-bytevector': () => new MutableBytevector(this, scm),
            bitvector: () => new Bitvector(this, scm),
            'mutable-bitvector': () => new MutableBitvector(this, scm),
            string: () => api.string_value(scm),
            'mutable-string': () => new MutableString(this, scm),
            procedure: () => new Procedure(this, scm),
            symbol: () => new Sym(this, scm),
            keyword: () => new Keyword(this, scm),
            variable: () => new Variable(this, scm),
            'atomic-box': () => new AtomicBox(this, scm),
            'hash-table': () => new HashTable(this, scm),
            'weak-table': () => new WeakTable(this, scm),
            fluid: () => new Fluid(this, scm),
            'dynamic-state': () => new DynamicState(this, scm),
            syntax: () => new Syntax(this, scm),
            'syntax-transformer': () => new SyntaxTransformer(this, scm),
            port: () => new Port(this, scm),
            struct: () => new Struct(this, scm),
            'extern-ref': () => api.extern_value(scm)
        };
        let handler = handlers[descr];
        return handler ? handler() : scm;
    }

    call(func, ...args) {
        let api = this.#instance.exports;
        let argv = api.make_vector(args.length + 1, api.scm_false());
        func = this.#to_scm(func);
        api.vector_set(argv, 0, func);
        for (let [idx, arg] of args.entries())
            api.vector_set(argv, idx + 1, this.#to_scm(arg));
        argv = api.call(func, argv);
        let results = [];
        for (let idx = 0; idx < api.vector_length(argv); idx++)
            results.push(this.#to_js(api.vector_ref(argv, idx)))
        return results;
    }

    call_async(func, ...args) {
        return new Promise((resolve, reject) => {
            this.call(func,
                      val => resolve(this.#to_js(val)),
                      err => reject(this.#to_js(err)),
                      ...args);
        })
    }

    car(x) { return this.#to_js(this.#instance.exports.car(x.obj)); }
    cdr(x) { return this.#to_js(this.#instance.exports.cdr(x.obj)); }

    vector_length(x) { return this.#instance.exports.vector_length(x.obj); }
    vector_ref(x, i) {
        return this.#to_js(this.#instance.exports.vector_ref(x.obj, i));
    }

    bytevector_length(x) {
        return this.#instance.exports.bytevector_length(x.obj);
    }
    bytevector_ref(x, i) {
        return this.#instance.exports.bytevector_ref(x.obj, i);
    }

    bitvector_length(x) {
        return this.#instance.exports.bitvector_length(x.obj);
    }
    bitvector_ref(x, i) {
        return this.#instance.exports.bitvector_ref(x.obj, i) == 1;
    }

    string_value(x) { return this.#instance.exports.string_value(x.obj); }
    symbol_name(x) { return this.#instance.exports.symbol_name(x.obj); }
    keyword_name(x) { return this.#instance.exports.keyword_name(x.obj); }
}

class SchemeTrapError extends Error {
    constructor(tag, data) { super(); this.tag = tag; this.data = data; }
    // FIXME: data is raw Scheme object; would need to be reflected to
    // have a toString.
    toString() { return `SchemeTrap(${this.tag}, <data>)`; }
}

class SchemeQuitError extends Error {
    constructor(status) { super(); this.status = status; }
    toString() { return `SchemeQuit(status=${this.status})`; }
}

function string_repr(str) {
    // FIXME: Improve to match Scheme.
    return '"' + str.replace(/(["\\])/g, '\\$1').replace(/\n/g, '\\n') + '"';
}

function flonum_to_string(f64) {
    if (Object.is(f64, -0)) {
        return '-0.0';
    } else if (Number.isFinite(f64)) {
        let repr = f64 + '';
        return /^-?[0-9]+$/.test(repr) ? repr + '.0' : repr;
    } else if (Number.isNaN(f64)) {
        return '+nan.0';
    } else {
        return f64 < 0 ? '-inf.0' : '+inf.0';
    }
}

let async_invoke = typeof queueMicrotask !== 'undefined'
    ? queueMicrotask
    : thunk => setTimeout(thunk, 0);
function async_invoke_later(thunk, jiffies) {
    setTimeout(thunk, jiffies / 1000);
}

let wtf8_helper;

function wtf8_to_string(wtf8) {
    let { as_iter, iter_next } = wtf8_helper.exports;
    let codepoints = [];
    let iter = as_iter(wtf8);
    for (let cp = iter_next(iter); cp != -1; cp = iter_next(iter))
        codepoints.push(cp);

    // Passing too many codepoints can overflow the stack.
    let maxcp = 100000;
    if (codepoints.length <= maxcp) {
        return String.fromCodePoint(...codepoints);
    }

    // For converting large strings, concatenate several smaller
    // strings.
    let substrings = [];
    let end = 0;
    for (let start = 0; start != codepoints.length; start = end) {
        end = Math.min(start + maxcp, codepoints.length);
        substrings.push(String.fromCodePoint(...codepoints.slice(start, end)));
    }
    return substrings.join('');
}

function string_to_wtf8(str) {
    let { string_builder, builder_push_codepoint, finish_builder } =
        wtf8_helper.exports;
    let builder = string_builder()
    for (let cp of str)
        builder_push_codepoint(builder, cp.codePointAt(0));
    return finish_builder(builder);
}

async function load_wtf8_helper_module(reflect_wasm_dir = '') {
    if (wtf8_helper) return;
    let wtf8_wasm = reflect_wasm_dir + "/wtf8.wasm";
    let { module, instance } = await instantiate_streaming(wtf8_wasm);
    wtf8_helper = instance;
}

function make_textual_writable_stream(write_chars) {
    const decoder = new TextDecoder("utf-8");
    return new WritableStream({
        write(chunk) {
            return new Promise((resolve, reject) => {
                write_chars(decoder.decode(chunk, { stream: true }));
                resolve();
            });
        }
    });
}

class SchemeModule {
    #instance;
    #io_handler;
    #debug_handler;
    #ffi_handler;
    #finalization_handler;
    static #rt = {
        bignum_from_string(str) { return BigInt(str); },
        bignum_from_i32(n) { return BigInt(n); },
        bignum_from_i64(n) { return n; },
        bignum_from_u64(n) { return n < 0n ? 0xffff_ffff_ffff_ffffn + (n + 1n) : n; },
        bignum_is_i64(n) {
            return -0x8000_0000_0000_0000n <= n && n <= 0x7FFF_FFFF_FFFF_FFFFn;
        },
        bignum_is_u64(n) {
            return 0n <= n && n <= 0xFFFF_FFFF_FFFF_FFFFn;
        },
        // This truncates; see https://tc39.es/ecma262/#sec-tobigint64.
        bignum_get_i64(n) { return n; },

        bignum_add(a, b) { return BigInt(a) + BigInt(b) },
        bignum_sub(a, b) { return BigInt(a) - BigInt(b) },
        bignum_mul(a, b) { return BigInt(a) * BigInt(b) },
        bignum_lsh(a, b) { return BigInt(a) << BigInt(b) },
        bignum_rsh(a, b) { return BigInt(a) >> BigInt(b) },
        bignum_quo(a, b) { return BigInt(a) / BigInt(b) },
        bignum_rem(a, b) { return BigInt(a) % BigInt(b) },
        bignum_mod(a, b) {
            let r = BigInt(a) % BigInt(b);
            if ((b > 0n && r < 0n) || (b < 0n && r > 0n)) {
                return b + r;
            } else {
                return r;
            }
        },
        bignum_gcd(a, b) {
            a = BigInt(a);
            b = BigInt(b);
            if (a < 0n) { a = -a; }
            if (b < 0n) { b = -b; }
            if (a == 0n) { return b; }
            if (b == 0n) { return a; }

            let r;
            while (b != 0n) {
                r = a % b;
                a = b;
                b = r;
            }
            return a;
        },

        bignum_logand(a, b) { return BigInt(a) & BigInt(b); },
        bignum_logior(a, b) { return BigInt(a) | BigInt(b); },
        bignum_logxor(a, b) { return BigInt(a) ^ BigInt(b); },
        bignum_logcount(a) {
            let c = 0;
            // Iterate over 32-bit chunks of the BigInt until all bits
            // are consumed.
            for(let b = a; b > 0n; b = b >> 32n) {
                let v = Number(BigInt.asUintN(32, b));
                // Bit shift magic from:
                // https://graphics.stanford.edu/~seander/bithacks.html#CountBitsSetParallel
                v = v - ((v >> 1) & 0x55555555);
                v = (v & 0x33333333) + ((v >> 2) & 0x33333333);
                c += ((v + (v >> 4) & 0xF0F0F0F) * 0x1010101) >> 24;
            }
            return c;
        },

        bignum_lt(a, b) { return a < b; },
        bignum_le(a, b) { return a <= b; },
        bignum_eq(a, b) { return a == b; },

        bignum_to_f64(n) { return Number(n); },

        flonum_to_string,

        string_upcase: Function.call.bind(String.prototype.toUpperCase),
        string_downcase: Function.call.bind(String.prototype.toLowerCase),

        make_weak_ref(x) { return new WeakRef(x); },
        weak_ref_deref(ref, fail) {
            const val = ref.deref();
            return val === undefined ? fail: val;
        },

        make_weak_map() { return new WeakMap; },
        weak_map_get(map, k, fail) {
            const val = map.get(k);
            return val === undefined ? fail: val;
        },
        weak_map_set(map, k, v) { return map.set(k, v); },
        weak_map_delete(map, k) { return map.delete(k); },

        fsqrt: Math.sqrt,
        fsin: Math.sin,
        fcos: Math.cos,
        ftan: Math.tan,
        fasin: Math.asin,
        facos: Math.acos,
        fatan: Math.atan,
        fatan2: Math.atan2,
        flog: Math.log,
        fexp: Math.exp,

        jiffies_per_second() { return 1000000; },
        current_jiffy() { return performance.now() * 1000; },
        current_second() { return Date.now() / 1000; },
        make_date(year, month, day, hour, min, sec, ms) {
            return new Date(year, month, day, hour, min, sec, ms);
        },
        ms_utc_to_date(t) { return new Date(t); },
        date_year(d) { return d.getFullYear(); },
        date_month(d) { return d.getMonth(); },
        date_day(d) { return d.getDate(); },
        date_weekday(d) { return d.getDay(); },
        date_hour(d) { return d.getHours(); },
        date_min(d) { return d.getMinutes(); },
        date_sec(d) { return d.getSeconds(); },
        date_ms(d) { return d.getMilliseconds(); },
        date_timezone_offset(d) { return d.getTimezoneOffset(); },
        date_locale(d) {
            return new Intl.DateTimeFormat(undefined, {
                dateStyle: "long",
                timeStyle: "long"
            }).format(d);
        },
        date_locale_date(d) {
            return new Intl.DateTimeFormat(undefined, {
                dateStyle: "long"
            }).format(d);
        },
        date_locale_time(d) {
            return new Intl.DateTimeFormat(undefined, {
                timeStyle: "long"
            }).format(d);
        },

        async_invoke,
        async_invoke_later,
        promise_on_completed(p, kt, kf) {
            p.then((val) => {
                if (val === undefined) {
                    kt(false);
                } else {
                    kt(val);
                }
            }, kf);
        },
        promise_complete(callback, val) { callback(val); },

        // Wrap in functions to allow for lazy loading of the wtf8
        // module.
        wtf8_to_string(wtf8) { return wtf8_to_string(wtf8); },
        string_to_wtf8(str) { return string_to_wtf8(str); },

        make_regexp(pattern, flags) { return new RegExp(pattern, flags); },
        regexp_exec(re, str) { return re.exec(str); },
        regexp_match_string(m) { return m.input; },
        regexp_match_start(m) { return m.index; },
        regexp_match_end(m) { return m.index + m[0].length; },
        regexp_match_count(m) { return m.length; },
        regexp_match_substring(m, i) {
            const str = m[i];
            if (str === undefined) {
                return null;
            }
            return str;
        },

        die(tag, data) { throw new SchemeTrapError(tag, data); },
        quit(status) { throw new SchemeQuitError(status); },

        stream_make_chunk(len) { return new Uint8Array(len); },
        stream_chunk_length(chunk) { return chunk.length; },
        stream_chunk_ref(chunk, idx) { return chunk[idx]; },
        stream_chunk_set(chunk, idx, val) { chunk[idx] = val; },
        stream_get_reader(stream) { return stream.getReader(); },
        stream_read(reader) { return reader.read(); },
        stream_result_chunk(result) { return result.value; },
        stream_result_done(result) { return result.done ? 1 : 0; },
        stream_get_writer(stream) { return stream.getWriter(); },
        stream_write(writer, chunk) { return writer.write(chunk); },
        stream_close_writer(writer) { return writer.close(); },
    };

    static #code_origins = new WeakMap;
    static #all_modules = new IterableWeakSet;
    static #code_origin(code) {
        if (SchemeModule.#code_origins.has(code))
            return SchemeModule.#code_origins.get(code);
        for (let mod of SchemeModule.#all_modules) {
            for (let i = 0, x = null; x = mod.instance_code(i); i++) {
                let origin = [mod, i];
                if (!SchemeModule.#code_origins.has(x))
                    SchemeModule.#code_origins.set(x, origin);
                if (x === code)
                    return origin;
            }
        }
        return [null, 0];
    }

    static #code_name(code) {
        let [mod, idx] = SchemeModule.#code_origin(code);
        if (mod)
            return mod.instance_code_name(idx);
        return null;
    }

    static #code_source(code) {
        let [mod, idx] = SchemeModule.#code_origin(code);
        if (mod)
            return mod.instance_code_source(idx);
        return [null, 0, 0];
    }

    constructor(instance) {
        SchemeModule.#all_modules.add(this);
        this.#instance = instance;
        let open_file_error = (filename) => {
            throw new Error('No file system access');
        };
        if (typeof printErr === 'function') { // v8/sm dev console
            // On the console, try to use 'write' (v8) or 'putstr' (sm),
            // as these don't add an extraneous newline.  Unfortunately
            // JSC doesn't have a printer that doesn't add a newline.
            let write_no_newline =
                typeof write === 'function' ? write
                : typeof putstr === 'function' ? putstr : print;
            // Use readline when available.  v8 strips newlines so
            // we need to add them back.
            let read_stdin =
                typeof readline == 'function' ? () => {
                    let line = readline();
                    if (line) {
                        return `${line}\n`;
                    } else {
                        return '\n';
                    }
                }: () => '';
            this.#io_handler = {
                write_stdout: write_no_newline,
                write_stderr: printErr,
                read_stdin,
                file_exists: (filename) => false,
                open_input_file: open_file_error,
                open_output_file: open_file_error,
                close_file: () => undefined,
                read_file: (handle, length) => 0,
                write_file: (handle, length) => 0,
                seek_file: (handle, offset, whence) => -1,
                file_random_access: (handle) => false,
                file_buffer_size: (handle) => 0,
                file_buffer_ref: (handle, i) => 0,
                file_buffer_set: (handle, i, x) => undefined,
                delete_file: (filename) => undefined,
                // FIXME: We should polyfill these out.
                stream_stdin() { throw new Error('stream_stdin not implemented'); },
                stream_stdout() { throw new Error('stream_stderr not implemented'); },
                stream_stderr() { throw new Error('stream_stderr not implemented'); },
            };
        } else if (typeof window !== 'undefined') { // web browser
            this.#io_handler = {
                write_stdout: console.log,
                write_stderr: console.error,
                read_stdin: () => '',
                file_exists: (filename) => false,
                open_input_file: open_file_error,
                open_output_file: open_file_error,
                close_file: () => undefined,
                read_file: (handle, length) => 0,
                write_file: (handle, length) => 0,
                seek_file: (handle, offset, whence) => -1,
                file_random_access: (handle) => false,
                file_buffer_size: (handle) => 0,
                file_buffer_ref: (handle, i) => 0,
                file_buffer_set: (handle, i, x) => undefined,
                delete_file: (filename) => undefined,
                stream_stdin() { return new ReadableStream; },
                stream_stdout() {
                    return make_textual_writable_stream(s => console.log(s));
                },
                stream_stderr() {
                    return make_textual_writable_stream(s => console.error(s));
	        },
            };
        } else { // nodejs
            const fs = require('fs');
            const process = require('process');
            const { ReadableStream, WritableStream } = require('node:stream/web');

            const bufLength = 1024;
            const stdinBuf = Buffer.alloc(bufLength);
            const SEEK_SET = 0, SEEK_CUR = 1, SEEK_END = 2;
            this.#io_handler = {
                write_stdout: process.stdout.write.bind(process.stdout),
                write_stderr: process.stderr.write.bind(process.stderr),
                read_stdin: () => {
                    let n = fs.readSync(process.stdin.fd, stdinBuf, 0, stdinBuf.length);
                    return stdinBuf.toString('utf8', 0, n);
                },
                file_exists: fs.existsSync.bind(fs),
                open_input_file: (filename) => {
                    let fd = fs.openSync(filename, 'r');
                    return {
                        fd,
                        buf: Buffer.alloc(bufLength),
                        pos: 0
                    };
                },
                open_output_file: (filename) => {
                    let fd = fs.openSync(filename, 'w');
                    return {
                        fd,
                        buf: Buffer.alloc(bufLength),
                        pos: 0
                    };
                },
                close_file: (handle) => {
                    fs.closeSync(handle.fd);
                },
                read_file: (handle, count) => {
                    const n = fs.readSync(handle.fd, handle.buf, 0, count, handle.pos);
                    handle.pos += n;
                    return n;
                },
                write_file: (handle, count) => {
                    const n = fs.writeSync(handle.fd, handle.buf, 0, count, handle.pos);
                    handle.pos += n;
                    return n;
                },
                seek_file: (handle, offset, whence) => {
                    // There doesn't seem to be a way to ask NodeJS if
                    // a position is valid or not.
                    if (whence == SEEK_SET) {
                        handle.pos = offset;
                        return handle.pos;
                    } else if (whence == SEEK_CUR) {
                        handle.pos += offset;
                        return handle.pos;
                    }

                    // SEEK_END not supported.
                    return -1;
                },
                file_random_access: (handle) => {
                    return true;
                },
                file_buffer_size: (handle) => {
                    return handle.buf.length;
                },
                file_buffer_ref: (handle, i) => {
                    return handle.buf[i];
                },
                file_buffer_set: (handle, i, x) => {
                    handle.buf[i] = x;
                },
                delete_file: fs.rmSync.bind(fs),
                stream_stdin() {
                    return new ReadableStream({
                        async start(controller) {
                            for await (const chunk of process.stdin) {
                                controller.enqueue(chunk);
                            }
                            controller.close();
                        }
                    });
                },
                stream_stdout() {
                    return make_textual_writable_stream(s => process.stdout.write(s));
                },
                stream_stderr() {
                    return make_textual_writable_stream(s => process.stderr.write(s));
	        },
            };
        }
        this.#debug_handler = {
            debug_str(x) { console.log(`debug: ${x}`); },
            debug_str_i32(x, y) { console.log(`debug: ${x}: ${y}`); },
            debug_str_scm(x, y) { console.log(`debug: ${x}: #<scm>`); },
        };
    }
    static async fetch_and_instantiate(path, { abi, reflect_wasm_dir = '.',
                                               user_imports = {} }) {
        await load_wtf8_helper_module(reflect_wasm_dir);
        let io = {
            write_stdout(str) { mod.#io_handler.write_stdout(str); },
            write_stderr(str) { mod.#io_handler.write_stderr(str); },
            read_stdin() { return mod.#io_handler.read_stdin(); },
            file_exists(filename) { return mod.#io_handler.file_exists(filename); },
            open_input_file(filename) { return mod.#io_handler.open_input_file(filename); },
            open_output_file(filename) { return mod.#io_handler.open_output_file(filename); },
            close_file(handle) { mod.#io_handler.close_file(handle); },
            read_file(handle, length) { return mod.#io_handler.read_file(handle, length); },
            write_file(handle, length) { return mod.#io_handler.write_file(handle, length); },
            seek_file(handle, offset, whence) { return mod.#io_handler.seek_file(handle, offset, whence); },
            file_random_access(handle) { return mod.#io_handler.file_random_access(handle); },
            file_buffer_size(handle) { return mod.#io_handler.file_buffer_size(handle); },
            file_buffer_ref(handle, i) { return mod.#io_handler.file_buffer_ref(handle, i); },
            file_buffer_set(handle, i, x) { return mod.#io_handler.file_buffer_set(handle, i, x); },
            delete_file(filename) { mod.#io_handler.delete_file(filename); },
            stream_stdin() { return mod.#io_handler.stream_stdin(); },
            stream_stdout() { return mod.#io_handler.stream_stdout(); },
            stream_stderr() { return mod.#io_handler.stream_stderr(); },
        };
        let debug = {
            debug_str(x) { mod.#debug_handler.debug_str(x); },
            debug_str_i32(x, y) { mod.#debug_handler.debug_str_i32(x, y); },
            debug_str_scm(x, y) { mod.#debug_handler.debug_str_scm(x, y); },
            code_name(code) { return SchemeModule.#code_name(code); },
            code_source(code) { return SchemeModule.#code_source(code); },
        };
        let ffi = {
            is_extern_func(x) {
                return mod.#ffi_handler.is_extern_func(x);
            },
            call_extern(f, args) {
                return mod.#ffi_handler.call_extern(f, args);
            },
            procedure_to_extern(proc) {
                return mod.#ffi_handler.procedure_to_extern(proc);
            }
        };
        let finalization = {
            make_finalization_registry(f) {
                return mod.#finalization_handler.make_finalization_registry(f);
            },
            finalization_registry_register(registry, target, heldValue) {
                mod.#finalization_handler.finalization_registry_register(registry, target, heldValue);
            },
            finalization_registry_register_with_token(registry, target, heldValue, unregisterToken) {
                mod.#finalization_handler.finalization_registry_register_with_token(registry, target, heldValue, unregisterToken);
            },
            finalization_registry_unregister(registry, unregisterToken) {
                return mod.#finalization_handler.finalization_registry_unregister(registry, unregisterToken);
            }
        };
        let imports = {
          rt: SchemeModule.#rt,
          abi, debug, io, ffi, finalization, ...user_imports
        };
        let { module, instance } = await instantiate_streaming(path, imports);
        let mod = new SchemeModule(instance);
        return mod;
    }
    set_io_handler(h) { this.#io_handler = h; }
    set_debug_handler(h) { this.#debug_handler = h; }
    set_ffi_handler(h) { this.#ffi_handler = h; }
    set_finalization_handler(h) { this.#finalization_handler = h; }
    all_exports() { return this.#instance.exports; }
    exported_abi() {
        let abi = {}
        for (let [k, v] of Object.entries(this.all_exports())) {
            if (k.startsWith("$"))
                abi[k] = v;
        }
        return abi;
    }
    exports() {
        let ret = {}
        for (let [k, v] of Object.entries(this.all_exports())) {
            if (!k.startsWith("$"))
                ret[k] = v;
        }
        return ret;
    }
    get_export(name) {
        if (name in this.all_exports())
            return this.all_exports()[name];
        throw new Error(`unknown export: ${name}`)
    }
    instance_code(idx) {
        if ('%instance-code' in this.all_exports()) {
            return this.all_exports()['%instance-code'](idx);
        }
        return null;
    }
    instance_code_name(idx) {
        if ('%instance-code-name' in this.all_exports()) {
            return this.all_exports()['%instance-code-name'](idx);
        }
        return null;
    }
    instance_code_source(idx) {
        if ('%instance-code-source' in this.all_exports()) {
            return this.all_exports()['%instance-code-source'](idx);
        }
        return [null, 0, 0];
    }
    async reflect(opts = {}) {
        return await Scheme.reflect(this.exported_abi(), opts);
    }
}

function repr(obj) {
    if (obj instanceof HeapObject)
        return obj.repr();
    if (typeof obj === 'boolean')
        return obj ? '#t' : '#f';
    if (typeof obj === 'number')
        return flonum_to_string(obj);
    if (typeof obj === 'string')
        return string_repr(obj);
    return obj + '';
}

// Modulize when possible.
if (typeof exports !== 'undefined') {
    exports.Scheme = Scheme;
    exports.SchemeQuitError = SchemeQuitError;
    exports.repr = repr;
}
