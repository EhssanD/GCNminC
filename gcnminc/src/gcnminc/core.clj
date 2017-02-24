(ns gcnminc.core
  (:gen-class))

(defn -main
  [& args]
  (let [input-filename (nth args 0)
        output-filename (nth args 1)
        lines (clojure.string/split (slurp input-filename) #"([\t ]*;[^\n]*)?[\t ]*\r?\n\t*") ; Convert consecutive white spaces into one white space.
        lines (remove #(re-find #"^$" %1) lines)
        lines (remove #(re-find #"^;" %1) lines)
        lines (map #(clojure.string/replace %1 #"[ \t]+" " ") lines)

        [metadata _ lines] (partition-by #(re-find #"^[\t ]*\.end_amdgpu_runtime_metadata" %1) lines) ; Drop unnecessary lines at the beginning of the file.

        ;metadata (remove #(not (re-find #"amd\.ArgSize" %1)) metadata)
        ;metadata (clojure.string/split (second metadata) #"  - ")
        metadata (remove #(not (re-find #"(amd\.KernelName|amd\.ArgSize)" %1)) metadata)
        metadata (remove #(re-find #"amd\.ArgKind: [789]," %1) metadata)
        metadata (map #(clojure.string/replace %1 #", amd\.Language: OpenCL C, amd\.LanguageVersion: \[ 1, 2 \], amd.Args:" "") metadata)
        metadata (map #(clojure.string/replace %1 #" *- \{ *" "") metadata)
        metadata (map #(clojure.string/replace %1 #"\} *" "") metadata)
        metadata (partition-by #(re-find #"amd\.KernelName" %1) metadata)
        metadata (map #(hash-map
                         (clojure.string/replace (first %1) #"^amd\.KernelName: ([^,]+).*$" "$1")
                         (map
                           (fn [arg index]
                             (let [[_ size align kind value-type type-name] (re-find #"^amd.ArgSize: ([^,]+), amd.ArgAlign: ([^,]+), amd.ArgKind: ([^,]+), amd.ArgValueType: ([^,]+), amd.ArgTypeName: '?([^,']+)'?," arg)
                                   value-type (case value-type
                                                "0" "structure"
                                                "1" "uchar"
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
                                   pointer? (if (re-find #"\*$" type-name) true false)
                                   structure? (= value-type "structure")]
                               (str
                                 "\t\t.arg arg" index ", "
                                 "\"" type-name "\", "
                                 value-type (if pointer? "*" "") (if (or pointer? structure?) ", ")
                                 (if structure? size)  (if (and structure? (or pointer?)) ", ")
                                 (if pointer? (str addr-qual))
                                 ;(if const? "const")
                                 )))
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
        lines (map #(clojure.string/replace %1 #"^gds_segment_byte_size =" ".gdssize") lines)
        ;lines (map #(clojure.string/replace %1 #"^.Lfunc_end.*:$" ".align 256") lines)

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
        lines (remove #(re-find #"^.text$" %1) lines)
        lines (remove #(re-find #"^.hsa_code_object_" %1) lines)
        lines (remove #(re-find #"^\.section \"\.note\.GNU-stack\"$" %1) lines)
        lines (remove #(re-find #"^(\.globl |\.p2align |\.ident |\.size )" %1) lines)

        kernels (partition-by #(= ".section .AMDGPU.csdata" %1) lines)
        kernels (remove #(= '(".section\t.AMDGPU.csdata") %1) kernels)
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
                                      (list
                                        "\t\t.arg _.global_offset_0, \"size_t\", long"
                                        "\t\t.arg _.global_offset_1, \"size_t\", long"
                                        "\t\t.arg _.global_offset_2, \"size_t\", long"
                                        "\t\t.arg _.printf_buffer, \"size_t\", void*, global, , rdonly"
                                        "\t\t.arg _.vqueue_pointer, \"size_t\", long"
                                        "\t\t.arg _.aqlwrap_pointer, \"size_t\", long")
                                      (get metadata (clojure.string/replace (first (first groups)) #"^\.kernel " ""))
                                      (list
                                        "\t.text"
                                        ;"\t\t\ts_mov_b32 m0, -1"
                                        )
                                      (map #(clojure.string/replace %1 #"^" "\t\t") (list (second (first groups))))
                                      (->> (nth groups 2 nil)
                                           (map #(clojure.string/replace %1 #"^" "\t\t"))
                                           (map #(clojure.string/replace %1 #"(flat_atomic_(add|sub)) ([^,]+), ([^,]+)$" "$1 $4, $3, $4"))
                                           (map #(clojure.string/replace %1 #"s_waitcnt$" "s_nop 0"))
                                           ;(map #(clojure.string/replace %1 #"_e(32|64) " " "))
                                           (map #(clojure.string/replace %1 #"^.*[^:]$" "\t$0"))
                                           (map #(clojure.string/replace %1 #"_lo_i32 " "_lo_u32 "))
                                           ))
                             lines (apply concat groups)]
                         lines))
                     kernels)
        ]
    (spit output-filename
          (apply str
                 (concat
                   (list
                     ".amdcl2\n"
                     ".gpu Ellesmere\n"
                     ".64bit\n"
                     ".driver_version 200406\n"
                     ".acl_version \"AMD-COMP-LIB-v0.8 (0.0.SC_BUILD_NUMBER)\"")
                   (apply str (map #(str %1 "\n") (apply concat kernels))))))))
