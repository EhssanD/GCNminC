(ns gcnminc.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.shell :refer [sh]])

  (:gen-class)
  (:import (java.net URLDecoder)))

(defn convert-llvm-output
  [gpu input-file-path output-file-path args]
  (let [lines (clojure.string/split (slurp input-file-path) #"([\t ]*;[^\n]*)?[\t ]*\r?\n\t*") ; Convert consecutive white spaces into one white space.
        lines (remove #(re-find #"^$" %1) lines)
        lines (remove #(re-find #"^;" %1) lines)
        lines (map #(clojure.string/replace %1 #"[ \t]+" " ") lines)

        [metadata _ lines] (partition-by #(re-find #"^[\t ]*\.end_amdgpu_runtime_metadata" %1) lines) ; Drop unnecessary lines at the beginning of the file.

        ;metadata (remove #(not (re-find #"amd\.ArgSize" %1)) metadata)
        ;metadata (clojure.string/split (second metadata) #"  - ")
        metadata (remove #(not (re-find #"(amd\.KernelName|amd\.ArgKind)" %1)) metadata)
        metadata (map #(clojure.string/replace %1 #", amd\.Language: OpenCL C, amd\.LanguageVersion: \[ 1, 2 \], amd.Args:" "") metadata)
        metadata (map #(clojure.string/replace %1 #" *- \{ *" "") metadata)
        metadata (map #(clojure.string/replace %1 #"\} *" "") metadata)
        metadata (partition-by #(re-find #"amd\.KernelName" %1) metadata)
        metadata (map #(hash-map
                         (clojure.string/replace (first %1) #"^amd\.KernelName: ([^,]+).*$" "$1")
                         (map
                           (fn [arg index]
                             (let [[_ size] (re-find #"amd.ArgSize: ([^,]+)" arg)
                                   [_ align] (re-find #"amd.ArgAlign: ([^,]+)" arg)
                                   [_ kind] (re-find #"amd.ArgKind: ([^,]+)" arg)
                                   [_ value-type] (re-find #"amd.ArgValueType: ([^,]+)" arg)
                                   [_ type-name] (re-find #"amd.ArgTypeName: '?([^,']+)'?" arg)
                                   value-type (case value-type
                                                "0" "structure"
                                                "1" "char"
                                                "2" "uchar"
                                                "6" "int"
                                                "7" "uint"
                                                "10" "ulong"
                                                value-type)
                                   [_ addr-qual] (re-find #"amd.ArgAddrQual: ([^, ]+)" arg)
                                   addr-qual (case addr-qual
                                               "0" "private"
                                               "1" "global"
                                               "2" "constant"
                                               "3" "local"
                                               nil)
                                   const? (if (re-find #"amd.ArgIsConst: 1" arg) true false)
                                   pointer? (if (and type-name (re-find #"\*$" type-name)) true false)
                                   structure? (= value-type "structure")]
                               (case kind
                                 "7" "\t\t.arg _.global_offset_0, \"size_t\", long"
                                 "8" "\t\t.arg _.global_offset_1, \"size_t\", long"
                                 "9" "\t\t.arg _.global_offset_2, \"size_t\", long"
                                 "11" "\t\t.arg _.printf_buffer, \"size_t\", void*, global, , rdonly"
                                 "12" "\t\t.arg _.vqueue_pointer, \"size_t\", long"
                                 "13" "\t\t.arg _.aqlwrap_pointer, \"size_t\", long"
                                 (str
                                   "\t\t.arg arg" index ", "
                                   "\"" type-name "\", "
                                   value-type (if pointer? "*" "") (if (or pointer? structure?) ", ")
                                   (if structure? size)  (if (and structure? (or pointer?)) ", ")
                                   (if pointer? (str addr-qual))
                                   ;(if const? "const")
                                   ))))
                           %2
                           (range)))
                      (take-nth 2 metadata)
                      (take-nth 2 (drop 1 metadata)))
        metadata (apply merge metadata)

        lines (map #(clojure.string/replace %1 #"^enable_sgpr_kernarg_segment_ptr = 1$" "/* $0 */\n\t\t.useargs") lines)
        lines (map #(clojure.string/replace %1 #"^enable_sgpr_dispatch_ptr = 1$" "/* $0 */\n\t\t.usesetup") lines)
        lines (map #(clojure.string/replace %1 #"^enable_sgpr_queue_ptr = 1$" "/* $0 */\n\t\t.useenqueue") lines)
        lines (map #(clojure.string/replace %1 #"^enable_sgpr_flat_scratch_init = 1$" "/* $0 */\n\t\t.usegeneric") lines)
        lines (map #(clojure.string/replace %1 #"^enable_sgpr_.*$" "/* $0 */") lines)
        lines (map #(clojure.string/replace %1 #"^user_sgpr_count = .*$" "/* $0 */") lines)
        lines (map #(clojure.string/replace %1 #"^\.amdgpu_hsa_kernel " ".kernel ") lines)
        lines (map #(clojure.string/replace %1 #"^enable_dx10_clamp = 1$" "/* $0 */\n\t\t.dx10clamp") lines)
        lines (map #(clojure.string/replace %1 #"^enable_ieee_mode = 1$" "/* $0 */\n\t\t.ieeemode") lines)
        lines (map #(clojure.string/replace %1 #"^float_mode = " ".floatmode ") lines)
        lines (map #(clojure.string/replace %1 #"^priv = 1$" ".priv") lines)
        lines (map #(clojure.string/replace %1 #"^priority = " ".priority ") lines)
        lines (map #(clojure.string/replace %1 #"^workitem_private_segment_byte_size =" ".scratchbuffer") lines)
        lines (map #(clojure.string/replace %1 #"^workgroup_group_segment_byte_size =" ".localsize") lines)
        lines (map #(clojure.string/replace %1 #"^gds_segment_byte_size = .*$" (str ".gdssize " (:gds-segment-size (:options args)))) lines)
        lines (map #(clojure.string/replace %1 #"^\.section \.AMDGPU\.csdata.*$" ".data") lines)
        lines (map #(clojure.string/replace %1 #"^\.section \.rodata.*$" ".globaldata") lines)
        lines (map #(clojure.string/replace %1 #"^s_add_u32 (s[0-9]+), (s[0-9]+), ([a-zA-Z_][a-zA-z_0-9\.]*)@(gotpc)?rel32@lo\+4" "s_mov_b32 $1, $3&0xffffffff") lines)
        lines (map #(clojure.string/replace %1 #"^s_addc_u32 (s[0-9]+), (s[0-9]+), ([a-zA-Z_][a-zA-z_0-9\.]*)@(gotpc)?rel32@hi\+4" "s_mov_b32 $1, $3>>32") lines)
        ;lines (map #(clojure.string/replace %1 #"^.Lfunc_end.*:$" ".align 256") lines)
        ;

        ;lines (remove #(re-find #"^[ \t]*;" %1) lines)
        ;lines (remove #(re-find #"^$" %1) lines)
        lines (remove #(re-find #"^\.type (.*),@function$" %1) lines)
        lines (remove #(re-find #"^amd_code_" %1) lines)
        lines (remove #(re-find #"^amd_machine_" %1) lines)
        lines (remove #(re-find #"^kernel_code_" %1) lines)
        ;lines (remove #(re-find #"^enable_sgpr_" %1) lines)
        lines (remove #(re-find #"^compute_pgm_rsrc" %1) lines)
        lines (remove #(re-find #"^debug_" %1) lines)
        lines (remove #(re-find #"^reserved_" %1) lines)
        lines (remove #(re-find #"^kernarg_" %1) lines)
        lines (remove #(re-find #"^is_" %1) lines)
        lines (remove #(re-find #"^enable_" %1) lines)
        lines (remove #(re-find #"^granulated_" %1) lines)

        lines (remove #(re-find #"^enable_ordered_append_gds" %1) lines)
        lines (remove #(re-find #"^max_scratch_backing_memory_byte_size" %1) lines)
        lines (remove #(re-find #"^private_element_size" %1) lines)
        lines (remove #(re-find #"^priv = 0$" %1) lines)
        lines (remove #(re-find #"^workgroup_fbarrier_count" %1) lines)
        lines (remove #(re-find #"^wavefront_sgpr_count" %1) lines)
        lines (remove #(re-find #"^workitem_vgpr_count" %1) lines)
        lines (remove #(re-find #"^group_segment_alignment" %1) lines)
        lines (remove #(re-find #"^private_segment_alignment" %1) lines)
        lines (remove #(re-find #"^call_convention" %1) lines)
        lines (remove #(re-find #"^wavefront_size" %1) lines)
        lines (remove #(re-find #"^runtime_loader_kernel_symbol" %1) lines)
        ;lines (remove #(re-find #"^.text$" %1) lines)
        lines (remove #(re-find #"^.hsa_code_object_" %1) lines)
        lines (remove #(re-find #"^\.section \"\.note\.GNU-stack\"$" %1) lines)
        lines (remove #(re-find #"^(\.globl |\.p2align |\.ident |\.size |\.type )" %1) lines)

        kernels (partition-by #(= ".text" %1) lines)
        kernels (remove #(= '(".text") %1) kernels)
        kernels (remove #(not (re-find #"^.kernel " (first %1))) kernels)

        kernels (map (fn [lines]
                       (let [groups (partition-by #(re-find #"^\.((end_)?amd_kernel_code_t|section \.AMDGPU\.runtime_metadata)$" %1) lines)
                             groups (remove #(re-find #"^\.((end_)?amd_kernel_code_t|section \.AMDGPU\.runtime_metadata)$" (first %1)) groups)
                             groups (list
                                      (list
                                        ""
                                        ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
                                        ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
                                        ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
                                        ""
                                        (first (first groups)))
                                      (list
                                        "\t.config"
                                        "\t\t.dims x"
                                        ; "\t\t.pgmrsrc2 0x00006040" ; Enable trap handlers.
                                        )
                                      (map #(clojure.string/replace %1 #"^" "\t\t") (second groups))
                                      ;(list
                                      ;  "\t\t.arg _.global_offset_0, \"size_t\", long"
                                      ;  "\t\t.arg _.global_offset_1, \"size_t\", long"
                                      ;  "\t\t.arg _.global_offset_2, \"size_t\", long"
                                      ;  "\t\t.arg _.printf_buffer, \"size_t\", void*, global, , rdonly"
                                      ;  "\t\t.arg _.vqueue_pointer, \"size_t\", long"
                                      ;  "\t\t.arg _.aqlwrap_pointer, \"size_t\", long")
                                      (get metadata (clojure.string/replace (first (first groups)) #"^\.kernel " ""))
                                      (list
                                        "\t.text"
                                        ;"\t\t\ts_mov_b32 m0, -1"
                                        )
                                      (map #(clojure.string/replace %1 #"^" "\t\t") (list (second (first groups))))
                                      (->> (nth groups 2 nil)
                                           (map #(clojure.string/replace %1 #"^" "\t\t"))
                                           (map #(clojure.string/replace %1 #"v_mov_b64[ \t]+v\[([0-9]+):([0-9]+)\],[ \t]+v\[([0-9]+):([0-9]+)\]" "v_mov_b32 v$1, v$3; v_mov_b32 v$2, v$4"))
                                           (map #(clojure.string/replace
                                                   %1
                                                   #"v_mov_b128[ \t]+v\[([0-9]+):([0-9]+)\],[ \t]+v\[([0-9]+):([0-9]+)\]"
                                                   (fn [result]
                                                      (str
                                                        "v_mov_b32 v" (nth result 1)             ", v" (nth result 3)             "; "
                                                        "v_mov_b32 v" (+ 1 (Integer. (nth result 1))) ", v" (+ 1 (Integer. (nth result 3))) "; "
                                                        "v_mov_b32 v" (+ 2 (Integer. (nth result 1))) ", v" (+ 2 (Integer. (nth result 3))) "; "
                                                        "v_mov_b32 v" (+ 3 (Integer. (nth result 1))) ", v" (+ 3 (Integer. (nth result 3)))))))
                                           (map #(clojure.string/replace
                                                   %1
                                                   #"([^a-zA-Z_0-9])v\[([0-9]+):([0-9]+)\]\.x"
                                                   (fn [result] (str (nth result 1) "v" (+ 0 (Integer. (nth result 2)))))))
                                           (map #(clojure.string/replace
                                                   %1
                                                   #"([^a-zA-Z_0-9])v\[([0-9]+):([0-9]+)\]\.y"
                                                   (fn [result] (str (nth result 1) "v" (+ 1 (Integer. (nth result 2)))))))
                                           (map #(clojure.string/replace
                                                   %1
                                                   #"([^a-zA-Z_0-9])v\[([0-9]+):([0-9]+)\]\.z"
                                                   (fn [result] (str (nth result 1) "v" (+ 2 (Integer. (nth result 2)))))))
                                           (map #(clojure.string/replace
                                                   %1
                                                   #"([^a-zA-Z_0-9])v\[([0-9]+):([0-9]+)\]\.w"
                                                   (fn [result] (str (nth result 1) "v" (+ 3 (Integer. (nth result 2)))))))
                                           (map #(clojure.string/replace %1 #"(flat_atomic_(add|sub)) ([^,]+), ([^,]+)$" "$1 $4, $3, $4"))
                                           (map #(clojure.string/replace %1 #"s_waitcnt$" "s_waitcnt vmcnt(0) lgkmcnt(0) expcnt(0)"))
                                           ;(map #(clojure.string/replace %1 #"_e(32|64) " " "))
                                           (map #(clojure.string/replace %1 #"^.*[^:]$" "\t$0"))
                                           (map #(clojure.string/replace %1 #"_lo_i32 " "_lo_u32 "))
                                           ))
                             lines (apply concat groups)]
                         lines))
                     kernels)
        ]
    (spit output-file-path
          (apply str
                 (concat
                   (list
                     ".amdcl2\n"
                     (str ".gpu " gpu"\n")
                     ".64bit\n"
                     ".driver_version 200406\n"
                     ".acl_version \"AMD-COMP-LIB-v0.8 (0.0.SC_BUILD_NUMBER)\"\n")
                   (apply str (map #(str %1 "\n") (apply concat kernels))))))))

(def cli-options
  ;; An option with a required argument
  [["-g" "--gpu GPU" "GPU"
    :default "Ellesmere"]
   ["-w" "--worksize WORKSIZE" "Worksize"
    :default 256]
   ["-v" "--verbose"]
   ["-d" "--debug"]
   ["-h" "--help"]
   ["-s" "--gds-segment-size GDS_SEGMENT_SIZE" "The size of the GDS segment for each vmid"
    :default 0]
   ["-O" "--optimization OPTIMIZATION" "Optimization level"
    :parse-fn #(Integer/parseInt %)
    :validate [#(<= 0 % 3) "Must be a number between 0 and 3"]
    :default "3"]
   ["-o" "--output OUTPUT" "The path of the output file"
     :default "a.out"]])

(defn -main
  [& args]
  (println "GCNminC, an LLVM/Clang-based offline OpenCL compiler for the AMD GCN architecture")
  (println "Copyright (c) 2017 zawawa @ bitcointalk.org")
  (try
    (let [args (clojure.tools.cli/parse-opts args cli-options)
          _ (when (:errors args)
              (print ((:errors args)))
              (System/exit 1))
          _ (when (or (:help args) (not (= 1 (count (:arguments args)))))
              (println "Usage: java -jar gcnminc.jar [-gwvdhsOo] input_file [-o output_file]")
              (println "Options:")
              (println (:summary args))
              (System/exit 0))
        jar-path (.getPath (.getLocation (.getCodeSource (.getProtectionDomain gcnminc.core))))
        jar-path (URLDecoder/decode jar-path "UTF-8")
        gcnminc-path (clojure.string/replace jar-path #"/[^/]+/[^/]+/[^/]+/[^/]+$" "")
        gcnminc-path (clojure.string/replace gcnminc-path #"^/([a-zA-Z]):/" "$1:/") ; For Windows
        _ (println gcnminc-path)
        gpu (:gpu (:options args))
        input-file-path (nth (:arguments args) 0)
        output-file-path (:output (:options args))
        llvm-output-file     (java.io.File/createTempFile "GCNminC-llvm-" ".asm")
        llvm-output-file-path (.getPath llvm-output-file)
        _                    (.delete llvm-output-file)
        gcnminc-output-file     (java.io.File/createTempFile "GCNminC-gcnminc-" ".asm")
        gcnminc-output-file-path (.getPath gcnminc-output-file)
        _                       (.delete gcnminc-output-file)
        clang-path (cond
                     (.exists (clojure.java.io/file (str gcnminc-path "/llvm-build/Release/bin/clang.EXE"))) (str gcnminc-path "/llvm-build/Release/bin/clang")
                     (.exists (clojure.java.io/file (str gcnminc-path "/llvm-build/Debug/bin/clang.EXE"))) (str gcnminc-path "/llvm-build/Debug/bin/clang")
                     (.exists (clojure.java.io/file (str gcnminc-path "/llvm-build/Release/bin/clang"))) (str gcnminc-path "/llvm-build/Release/bin/clang")
                     (.exists (clojure.java.io/file (str gcnminc-path "/llvm-build/Debug/bin/clang"))) (str gcnminc-path "/llvm-build/Debug/bin/clang")
                     (.exists (clojure.java.io/file (str gcnminc-path "/llvm-build/bin/clang"))) (str gcnminc-path "/llvm-build/bin/clang")
                     (.exists (clojure.java.io/file (str gcnminc-path "/bin/clang.EXE"))) (str gcnminc-path "/bin/clang")
                     :else (str gcnminc-path "/bin/clang")
                     )
        clrxasm-path (cond
                     (.exists (clojure.java.io/file (str gcnminc-path "/CLRadeonExtender/build/programs/Release/clrxasm.EXE"))) (str gcnminc-path "/CLRadeonExtender/build/programs/Release/clrxasm.EXE")
                     :else (str gcnminc-path "/CLRadeonExtender/build/programs/clrxasm")
                     )
        libclc-path (str gcnminc-path "/libclc/built_libs/amdgcn--amdhsa.bc")
                     ]
    (println (str "=== Compiling OpenCL source code: "  input-file-path " ==="))
    (if (:debug (:options args))
      (println "libclc-path:" libclc-path))
    (print (:err
               (clojure.java.shell/sh
                 clang-path
                 "-target" "amdgcn--amdhsa"
                 (case (:gpu (:options args))
                   "Ellesmere" "-mcpu=gfx804"
                   "ellesmere" "-mcpu=gfx804"
                   "-mcpu=gfx804")
                 (str "-I" gcnminc-path "/libclc/generic/include")
                 "-include" (str gcnminc-path "/libclc/generic/include/clc/clc.h")
                 "-Dcl_clang_storage_class_specifiers "
                 "-x" "cl"
                 "-std=CL2.0"
                 input-file-path
                 "-S"
                 "-o" llvm-output-file-path
                 "-Xclang"
                 "-mlink-bitcode-file"
                 "-Xclang"
                 libclc-path
                 "-fno-builtin"
                 "-D__GCNMINC__"

                 "-D__OPENCL_VERSION__=120"
                 (case (:gpu (:options args))
                   "Ellesmere" "-D__GCN3__"
                   "ellesmere" "-D__GCN3__"
                   "-D__GCN3__")
                 (str "-DWORKSIZE=" (:worksize (:options args)))
                 (str "-DGDS_SEGMENT_SIZE=" (:gds-segment-size (:options args)))
                 (str "-O" (:optimization (:options args)))
                 :dir (System/getProperty "user.dir")
                 )))
    (println (str "=== Converting LLVM output: "  llvm-output-file-path " ==="))
    (println (str "=== Generated CLRX input: "  gcnminc-output-file-path " ==="))
    (convert-llvm-output gpu llvm-output-file-path gcnminc-output-file-path args)
    (println (str "=== Generating OpenCL binary: " output-file-path " ==="))
    (println (:err
      (clojure.java.shell/sh
       clrxasm-path
       gcnminc-output-file-path
       "-o" output-file-path))))
    (catch Throwable e
           (println (str e))
           ))
  (shutdown-agents))
