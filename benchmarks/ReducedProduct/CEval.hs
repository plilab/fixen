module ReducedProduct.CEval where

import ReducedProduct.FixenNoPriorities qualified as NP
import ReducedProduct.FixenWithPriorities qualified as WP
import ReducedProduct.Hand qualified as H
import Prelude hiding (Eq, Num)

handTest :: [H.PS]
handTest =
  [ H.mkVar 0 "_PyEval_EvalFrameDefault"
  , H.mkSeq 0 1
  , H.mkVar 1 "tstate"
  , H.mkSeq 1 2
  , H.mkVar 2 "frame"
  , H.mkSeq 2 3
  , H.mkVar 3 "throwflag"
  , H.mkSeq 3 4
  , H.mkVar 4 "opcode"
  , H.mkSeq 4 5
  , H.mkVar 5 "oparg"
  , H.mkSeq 5 6
  , H.mkVar 6 "entry"
  , H.mkSeq 6 7
  , H.mkCond 7 (H.Eq (H.Num 0) (H.Num 1)) 9 10
  , H.mkAssign 9 "return" (H.Num 0)
  , H.mkSeq 9 11
  , H.mkVar 10 "NOP_10"
  , H.mkSeq 10 11
  , H.mkVar 11 "IF_ELSE_FOOTER"
  , H.mkVar 12 "next_instr"
  , H.mkSeq 12 13
  , H.mkVar 13 "stack_pointer"
  , H.mkSeq 13 14
  , H.mkAssign 14 "undefed" (H.Num 0)
  , H.mkSeq 14 15
  , H.mkAssign 15 "undefed" (H.Num 0)
  , H.mkSeq 15 16
  , H.mkAssign 16 "undefed" (H.Num 0)
  , H.mkSeq 16 17
  , H.mkAssign 17 "undefed" (H.Num 0)
  , H.mkSeq 17 18
  , H.mkAssign 18 "undefed" (H.Num 0)
  , H.mkSeq 18 19
  , H.mkAssign 19 "undefed" (H.Num 0)
  , H.mkSeq 19 20
  , H.mkAssign 20 "undefed" (H.Num 0)
  , H.mkSeq 20 21
  , H.mkAssign 21 "undefed" (H.Num 0)
  , H.mkSeq 21 22
  , H.mkAssign 22 "undefed" (H.Num 0)
  , H.mkSeq 22 23
  , H.mkAssign 23 "undefed" (H.Num 0)
  , H.mkSeq 23 24
  , H.mkAssign 24 "undefed" (H.Num 0)
  , H.mkSeq 24 25
  , H.mkCond 25 (H.Eq (H.Id "throwflag") (H.Num 1)) 27 33
  , H.mkCond 27 (H.Eq (H.Num 0) (H.Num 1)) 29 29
  , H.mkSeq 28 262
  , H.mkSeq 28 30
  , H.mkVar 29 "NOP_29"
  , H.mkSeq 29 30
  , H.mkVar 30 "IF_ELSE_FOOTER"
  , H.mkAssign 31 "next_instr" (H.Num 0)
  , H.mkSeq 31 32
  , H.mkAssign 32 "stack_pointer" (H.Num 0)
  , H.mkSeq 32 33
  , H.mkSeq 32 177
  , H.mkSeq 32 34
  , H.mkVar 33 "NOP_33"
  , H.mkSeq 33 34
  , H.mkVar 34 "IF_ELSE_FOOTER"
  , H.mkSeq 34 246
  , H.mkVar 35 "__CLABEL_dispatch_opcode"
  , H.mkSeq 35 36
  , H.mkCond 36 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 38 87
  , H.mkVar 38 "NOP_38"
  , H.mkVar 39 "__CLABEL_TARGET_BINARY_OP"
  , H.mkSeq 39 40
  , H.mkAssign 40 "undefed" (H.Num 0)
  , H.mkSeq 40 41
  , H.mkAssign 41 "next_instr" (H.Num 0)
  , H.mkSeq 41 42
  , H.mkVar 42 "__CLABEL_PREDICTED_BINARY_OP"
  , H.mkSeq 42 43
  , H.mkVar 43 "NOP_43"
  , H.mkVar 44 "this_instr"
  , H.mkSeq 44 45
  , H.mkVar 45 "lhs"
  , H.mkSeq 45 46
  , H.mkVar 46 "rhs"
  , H.mkSeq 46 47
  , H.mkVar 47 "res"
  , H.mkSeq 47 48
  , H.mkAssign 48 "rhs" (H.Num 0)
  , H.mkSeq 48 49
  , H.mkAssign 49 "lhs" (H.Num 0)
  , H.mkSeq 49 50
  , H.mkVar 50 "counter"
  , H.mkSeq 50 51
  , H.mkCond 51 (H.Eq (H.Num 0) (H.Num 1)) 53 56
  , H.mkAssign 53 "next_instr" (H.Num 0)
  , H.mkSeq 53 54
  , H.mkAssign 54 "stack_pointer" (H.Num 0)
  , H.mkSeq 54 55
  , H.mkAssign 55 "opcode" (H.Num 0)
  , H.mkSeq 55 56
  , H.mkSeq 55 35
  , H.mkSeq 55 57
  , H.mkVar 56 "NOP_56"
  , H.mkSeq 56 57
  , H.mkVar 57 "IF_ELSE_FOOTER"
  , H.mkAssign 58 "undefed" (H.Num 0)
  , H.mkSeq 58 59
  , H.mkCond 59 (H.Eq (H.Num 0) (H.Num 1)) 60 61
  , H.mkAssign 60 "undefed" (H.Num 0)
  , H.mkSeq 60 59
  , H.mkVar 61 "LOOP_FOOTER"
  , H.mkSeq 61 62
  , H.mkVar 62 "lhs_o"
  , H.mkSeq 62 63
  , H.mkVar 63 "rhs_o"
  , H.mkSeq 63 64
  , H.mkVar 64 "res_o"
  , H.mkSeq 64 65
  , H.mkAssign 65 "stack_pointer" (H.Num 0)
  , H.mkSeq 65 66
  , H.mkCond 66 (H.Eq (H.Plus (H.Id "res_o") (H.Num 0)) (H.Num 1)) 68 68
  , H.mkSeq 67 177
  , H.mkSeq 67 69
  , H.mkVar 68 "NOP_68"
  , H.mkSeq 68 69
  , H.mkVar 69 "IF_ELSE_FOOTER"
  , H.mkAssign 70 "res" (H.Num 0)
  , H.mkSeq 70 71
  , H.mkVar 71 "tmp"
  , H.mkSeq 71 72
  , H.mkAssign 72 "lhs" (H.Num 0)
  , H.mkSeq 72 73
  , H.mkAssign 73 "undefed" (H.Num 0)
  , H.mkSeq 73 74
  , H.mkAssign 74 "tmp" (H.Num 0)
  , H.mkSeq 74 75
  , H.mkAssign 75 "rhs" (H.Num 0)
  , H.mkSeq 75 76
  , H.mkAssign 76 "undefed" (H.Num 0)
  , H.mkSeq 76 77
  , H.mkAssign 77 "stack_pointer" (H.Num 0)
  , H.mkSeq 77 78
  , H.mkAssign 78 "stack_pointer" (H.Num 0)
  , H.mkSeq 78 79
  , H.mkVar 79 "word"
  , H.mkSeq 79 80
  , H.mkAssign 80 "opcode" (H.Num 0)
  , H.mkSeq 80 81
  , H.mkAssign 81 "oparg" (H.Num 0)
  , H.mkSeq 81 82
  , H.mkCond 82 (H.Eq (H.Num 0) (H.Num 1)) 83 86
  , H.mkVar 83 "word"
  , H.mkSeq 83 84
  , H.mkAssign 84 "opcode" (H.Num 0)
  , H.mkSeq 84 85
  , H.mkAssign 85 "oparg" (H.Num 0)
  , H.mkSeq 85 86
  , H.mkSeq 85 82
  , H.mkVar 86 "LOOP_FOOTER"
  , H.mkSeq 86 87
  , H.mkSeq 86 35
  , H.mkCond 87 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 89 129
  , H.mkVar 89 "NOP_89"
  , H.mkVar 90 "__CLABEL_TARGET_BINARY_OP_ADD_FLOAT"
  , H.mkSeq 90 91
  , H.mkVar 91 "this_instr"
  , H.mkSeq 91 92
  , H.mkAssign 92 "undefed" (H.Num 0)
  , H.mkSeq 92 93
  , H.mkAssign 93 "next_instr" (H.Num 0)
  , H.mkSeq 93 94
  , H.mkVar 94 "value"
  , H.mkSeq 94 95
  , H.mkVar 95 "left"
  , H.mkSeq 95 96
  , H.mkVar 96 "right"
  , H.mkSeq 96 97
  , H.mkVar 97 "res"
  , H.mkSeq 97 98
  , H.mkAssign 98 "value" (H.Num 0)
  , H.mkSeq 98 99
  , H.mkVar 99 "value_o"
  , H.mkSeq 99 100
  , H.mkCond 100 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Plus (H.Id "PyFloat_Type") (H.Num 0))) (H.Num 0)) (H.Num 1)) 102 102
  , H.mkSeq 101 42
  , H.mkSeq 101 103
  , H.mkVar 102 "NOP_102"
  , H.mkSeq 102 103
  , H.mkVar 103 "IF_ELSE_FOOTER"
  , H.mkAssign 104 "left" (H.Num 0)
  , H.mkSeq 104 105
  , H.mkVar 105 "left_o"
  , H.mkSeq 105 106
  , H.mkCond 106 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Plus (H.Id "PyFloat_Type") (H.Num 0))) (H.Num 0)) (H.Num 1)) 108 108
  , H.mkSeq 107 42
  , H.mkSeq 107 109
  , H.mkVar 108 "NOP_108"
  , H.mkSeq 108 109
  , H.mkVar 109 "IF_ELSE_FOOTER"
  , H.mkAssign 110 "right" (H.Num 0)
  , H.mkSeq 110 111
  , H.mkVar 111 "left_o"
  , H.mkSeq 111 112
  , H.mkVar 112 "right_o"
  , H.mkSeq 112 113
  , H.mkVar 113 "dres"
  , H.mkSeq 113 114
  , H.mkAssign 114 "res" (H.Num 0)
  , H.mkSeq 114 115
  , H.mkCond 115 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 117 117
  , H.mkSeq 116 173
  , H.mkSeq 116 118
  , H.mkVar 117 "NOP_117"
  , H.mkSeq 117 118
  , H.mkVar 118 "IF_ELSE_FOOTER"
  , H.mkAssign 119 "undefed" (H.Num 0)
  , H.mkSeq 119 120
  , H.mkAssign 120 "stack_pointer" (H.Num 0)
  , H.mkSeq 120 121
  , H.mkVar 121 "word"
  , H.mkSeq 121 122
  , H.mkAssign 122 "opcode" (H.Num 0)
  , H.mkSeq 122 123
  , H.mkAssign 123 "oparg" (H.Num 0)
  , H.mkSeq 123 124
  , H.mkCond 124 (H.Eq (H.Num 0) (H.Num 1)) 125 128
  , H.mkVar 125 "word"
  , H.mkSeq 125 126
  , H.mkAssign 126 "opcode" (H.Num 0)
  , H.mkSeq 126 127
  , H.mkAssign 127 "oparg" (H.Num 0)
  , H.mkSeq 127 128
  , H.mkSeq 127 124
  , H.mkVar 128 "LOOP_FOOTER"
  , H.mkSeq 128 129
  , H.mkSeq 128 35
  , H.mkCond 129 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 131 170
  , H.mkVar 131 "NOP_131"
  , H.mkVar 132 "__CLABEL_TARGET_BINARY_OP_ADD_INT"
  , H.mkSeq 132 133
  , H.mkVar 133 "this_instr"
  , H.mkSeq 133 134
  , H.mkAssign 134 "undefed" (H.Num 0)
  , H.mkSeq 134 135
  , H.mkAssign 135 "next_instr" (H.Num 0)
  , H.mkSeq 135 136
  , H.mkVar 136 "value"
  , H.mkSeq 136 137
  , H.mkVar 137 "left"
  , H.mkSeq 137 138
  , H.mkVar 138 "right"
  , H.mkSeq 138 139
  , H.mkVar 139 "res"
  , H.mkSeq 139 140
  , H.mkAssign 140 "value" (H.Num 0)
  , H.mkSeq 140 141
  , H.mkVar 141 "value_o"
  , H.mkSeq 141 142
  , H.mkCond 142 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 144 144
  , H.mkSeq 143 42
  , H.mkSeq 143 145
  , H.mkVar 144 "NOP_144"
  , H.mkSeq 144 145
  , H.mkVar 145 "IF_ELSE_FOOTER"
  , H.mkAssign 146 "left" (H.Num 0)
  , H.mkSeq 146 147
  , H.mkVar 147 "left_o"
  , H.mkSeq 147 148
  , H.mkCond 148 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 150 150
  , H.mkSeq 149 42
  , H.mkSeq 149 151
  , H.mkVar 150 "NOP_150"
  , H.mkSeq 150 151
  , H.mkVar 151 "IF_ELSE_FOOTER"
  , H.mkAssign 152 "right" (H.Num 0)
  , H.mkSeq 152 153
  , H.mkVar 153 "left_o"
  , H.mkSeq 153 154
  , H.mkVar 154 "right_o"
  , H.mkSeq 154 155
  , H.mkAssign 155 "res" (H.Num 0)
  , H.mkSeq 155 156
  , H.mkCond 156 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 158 158
  , H.mkSeq 157 42
  , H.mkSeq 157 159
  , H.mkVar 158 "NOP_158"
  , H.mkSeq 158 159
  , H.mkVar 159 "IF_ELSE_FOOTER"
  , H.mkAssign 160 "undefed" (H.Num 0)
  , H.mkSeq 160 161
  , H.mkAssign 161 "stack_pointer" (H.Num 0)
  , H.mkSeq 161 162
  , H.mkVar 162 "word"
  , H.mkSeq 162 163
  , H.mkAssign 163 "opcode" (H.Num 0)
  , H.mkSeq 163 164
  , H.mkAssign 164 "oparg" (H.Num 0)
  , H.mkSeq 164 165
  , H.mkCond 165 (H.Eq (H.Num 0) (H.Num 1)) 166 169
  , H.mkVar 166 "word"
  , H.mkSeq 166 167
  , H.mkAssign 167 "opcode" (H.Num 0)
  , H.mkSeq 167 168
  , H.mkAssign 168 "oparg" (H.Num 0)
  , H.mkSeq 168 169
  , H.mkSeq 168 165
  , H.mkVar 169 "LOOP_FOOTER"
  , H.mkSeq 169 170
  , H.mkSeq 169 35
  , H.mkVar 170 "NOP_170"
  , H.mkSeq 170 171
  , H.mkVar 171 "__CLABEL_CODEGEN_SWITCH_EXIT_0"
  , H.mkSeq 171 172
  , H.mkVar 172 "NOP_172"
  , H.mkVar 173 "__CLABEL_pop_2_error"
  , H.mkSeq 173 174
  , H.mkAssign 174 "stack_pointer" (H.Num 0)
  , H.mkSeq 174 175
  , H.mkSeq 174 177
  , H.mkVar 175 "__CLABEL_pop_1_error"
  , H.mkSeq 175 176
  , H.mkAssign 176 "stack_pointer" (H.Num 0)
  , H.mkSeq 176 177
  , H.mkSeq 176 177
  , H.mkVar 177 "__CLABEL_error"
  , H.mkSeq 177 178
  , H.mkCond 178 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 180 181
  , H.mkAssign 180 "stack_pointer" (H.Num 0)
  , H.mkSeq 180 182
  , H.mkVar 181 "NOP_181"
  , H.mkSeq 181 182
  , H.mkVar 182 "IF_ELSE_FOOTER"
  , H.mkCond 183 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 185 192
  , H.mkVar 185 "f"
  , H.mkSeq 185 186
  , H.mkAssign 186 "stack_pointer" (H.Num 0)
  , H.mkSeq 186 187
  , H.mkCond 187 (H.Eq (H.Plus (H.Id "f") (H.Num 0)) (H.Num 1)) 189 190
  , H.mkAssign 189 "stack_pointer" (H.Num 0)
  , H.mkSeq 189 191
  , H.mkVar 190 "NOP_190"
  , H.mkSeq 190 191
  , H.mkVar 191 "IF_ELSE_FOOTER"
  , H.mkSeq 191 193
  , H.mkVar 192 "NOP_192"
  , H.mkSeq 192 193
  , H.mkVar 193 "IF_ELSE_FOOTER"
  , H.mkSeq 193 194
  , H.mkVar 194 "__CLABEL_exception_unwind"
  , H.mkSeq 194 195
  , H.mkVar 195 "offset"
  , H.mkSeq 195 196
  , H.mkVar 196 "level"
  , H.mkSeq 196 197
  , H.mkVar 197 "handler"
  , H.mkSeq 197 198
  , H.mkVar 198 "lasti"
  , H.mkSeq 198 199
  , H.mkVar 199 "handled"
  , H.mkSeq 199 200
  , H.mkCond 200 (H.Eq (H.Plus (H.Id "handled") (H.Num 0)) (H.Num 1)) 202 206
  , H.mkVar 202 "stackbase"
  , H.mkSeq 202 203
  , H.mkCond 203 (H.Eq (H.Plus (H.Num 0) (H.Id "stackbase")) (H.Num 1)) 204 205
  , H.mkVar 204 "ref"
  , H.mkSeq 204 205
  , H.mkSeq 204 203
  , H.mkVar 205 "LOOP_FOOTER"
  , H.mkSeq 205 206
  , H.mkSeq 205 234
  , H.mkSeq 205 207
  , H.mkVar 206 "NOP_206"
  , H.mkSeq 206 207
  , H.mkVar 207 "IF_ELSE_FOOTER"
  , H.mkVar 208 "new_top"
  , H.mkSeq 208 209
  , H.mkCond 209 (H.Eq (H.Plus (H.Num 0) (H.Id "new_top")) (H.Num 1)) 210 211
  , H.mkVar 210 "ref"
  , H.mkSeq 210 211
  , H.mkSeq 210 209
  , H.mkVar 211 "LOOP_FOOTER"
  , H.mkSeq 211 212
  , H.mkCond 212 (H.Eq (H.Id "lasti") (H.Num 1)) 214 216
  , H.mkVar 214 "frame_lasti"
  , H.mkSeq 214 215
  , H.mkVar 215 "lasti"
  , H.mkSeq 215 216
  , H.mkSeq 215 217
  , H.mkVar 216 "NOP_216"
  , H.mkSeq 216 217
  , H.mkVar 217 "IF_ELSE_FOOTER"
  , H.mkVar 218 "exc"
  , H.mkSeq 218 219
  , H.mkAssign 219 "next_instr" (H.Num 0)
  , H.mkSeq 219 220
  , H.mkVar 220 "err"
  , H.mkSeq 220 221
  , H.mkCond 221 (H.Eq (H.Plus (H.Id "err") (H.Num 0)) (H.Num 1)) 223 223
  , H.mkSeq 222 194
  , H.mkSeq 222 224
  , H.mkVar 223 "NOP_223"
  , H.mkSeq 223 224
  , H.mkVar 224 "IF_ELSE_FOOTER"
  , H.mkAssign 225 "stack_pointer" (H.Num 0)
  , H.mkSeq 225 226
  , H.mkVar 226 "word"
  , H.mkSeq 226 227
  , H.mkAssign 227 "opcode" (H.Num 0)
  , H.mkSeq 227 228
  , H.mkAssign 228 "oparg" (H.Num 0)
  , H.mkSeq 228 229
  , H.mkCond 229 (H.Eq (H.Num 0) (H.Num 1)) 230 233
  , H.mkVar 230 "word"
  , H.mkSeq 230 231
  , H.mkAssign 231 "opcode" (H.Num 0)
  , H.mkSeq 231 232
  , H.mkAssign 232 "oparg" (H.Num 0)
  , H.mkSeq 232 233
  , H.mkSeq 232 229
  , H.mkVar 233 "LOOP_FOOTER"
  , H.mkSeq 233 234
  , H.mkSeq 233 35
  , H.mkVar 234 "__CLABEL_exit_unwind"
  , H.mkSeq 234 235
  , H.mkVar 235 "dying"
  , H.mkSeq 235 236
  , H.mkAssign 236 "frame" (H.Num 0)
  , H.mkSeq 236 237
  , H.mkAssign 237 "undefed" (H.Num 0)
  , H.mkSeq 237 238
  , H.mkCond 238 (H.Eq (H.Plus (H.Num 0) (H.Id "FRAME_OWNED_BY_INTERPRETER")) (H.Num 1)) 240 242
  , H.mkAssign 240 "undefed" (H.Num 0)
  , H.mkSeq 240 241
  , H.mkAssign 241 "return" (H.Num 0)
  , H.mkSeq 241 242
  , H.mkSeq 241 243
  , H.mkVar 242 "NOP_242"
  , H.mkSeq 242 243
  , H.mkVar 243 "IF_ELSE_FOOTER"
  , H.mkAssign 244 "next_instr" (H.Num 0)
  , H.mkSeq 244 245
  , H.mkAssign 245 "stack_pointer" (H.Num 0)
  , H.mkSeq 245 246
  , H.mkSeq 245 177
  , H.mkVar 246 "__CLABEL_start_frame"
  , H.mkSeq 246 247
  , H.mkVar 247 "too_deep"
  , H.mkSeq 247 248
  , H.mkCond 248 (H.Eq (H.Id "too_deep") (H.Num 1)) 250 250
  , H.mkSeq 249 234
  , H.mkSeq 249 251
  , H.mkVar 250 "NOP_250"
  , H.mkSeq 250 251
  , H.mkVar 251 "IF_ELSE_FOOTER"
  , H.mkAssign 252 "next_instr" (H.Num 0)
  , H.mkSeq 252 253
  , H.mkAssign 253 "stack_pointer" (H.Num 0)
  , H.mkSeq 253 254
  , H.mkVar 254 "word"
  , H.mkSeq 254 255
  , H.mkAssign 255 "opcode" (H.Num 0)
  , H.mkSeq 255 256
  , H.mkAssign 256 "oparg" (H.Num 0)
  , H.mkSeq 256 257
  , H.mkCond 257 (H.Eq (H.Num 0) (H.Num 1)) 258 261
  , H.mkVar 258 "word"
  , H.mkSeq 258 259
  , H.mkAssign 259 "opcode" (H.Num 0)
  , H.mkSeq 259 260
  , H.mkAssign 260 "oparg" (H.Num 0)
  , H.mkSeq 260 261
  , H.mkSeq 260 257
  , H.mkVar 261 "LOOP_FOOTER"
  , H.mkSeq 261 262
  , H.mkSeq 261 35
  , H.mkVar 262 "__CLABEL_early_exit"
  , H.mkSeq 262 263
  , H.mkVar 263 "NOP_263"
  , H.mkVar 264 "dying"
  , H.mkSeq 264 265
  , H.mkAssign 265 "frame" (H.Num 0)
  , H.mkSeq 265 266
  , H.mkAssign 266 "undefed" (H.Num 0)
  , H.mkSeq 266 267
  , H.mkAssign 267 "undefed" (H.Num 0)
  , H.mkSeq 267 268
  , H.mkAssign 268 "return" (H.Num 0)
  , H.mkSeq 268 269
  , H.mkSeq 268 269
  , H.mkVar 269 "PROG_END"
  ]

noPrioritiesTest :: [NP.Fact]
noPrioritiesTest =
  [ NP.Var 0 "_PyEval_EvalFrameDefault"
  , NP.Seq 0 1
  , NP.Var 1 "tstate"
  , NP.Seq 1 2
  , NP.Var 2 "frame"
  , NP.Seq 2 3
  , NP.Var 3 "throwflag"
  , NP.Seq 3 4
  , NP.Var 4 "opcode"
  , NP.Seq 4 5
  , NP.Var 5 "oparg"
  , NP.Seq 5 6
  , NP.Var 6 "entry"
  , NP.Seq 6 7
  , NP.Cond 7 (NP.Eq (NP.Num 0) (NP.Num 1)) 9 10
  , NP.Assign 9 "return" (NP.Num 0)
  , NP.Seq 9 11
  , NP.Var 10 "NOP_10"
  , NP.Seq 10 11
  , NP.Var 11 "IF_ELSE_FOOTER"
  , NP.Var 12 "next_instr"
  , NP.Seq 12 13
  , NP.Var 13 "stack_pointer"
  , NP.Seq 13 14
  , NP.Assign 14 "undefed" (NP.Num 0)
  , NP.Seq 14 15
  , NP.Assign 15 "undefed" (NP.Num 0)
  , NP.Seq 15 16
  , NP.Assign 16 "undefed" (NP.Num 0)
  , NP.Seq 16 17
  , NP.Assign 17 "undefed" (NP.Num 0)
  , NP.Seq 17 18
  , NP.Assign 18 "undefed" (NP.Num 0)
  , NP.Seq 18 19
  , NP.Assign 19 "undefed" (NP.Num 0)
  , NP.Seq 19 20
  , NP.Assign 20 "undefed" (NP.Num 0)
  , NP.Seq 20 21
  , NP.Assign 21 "undefed" (NP.Num 0)
  , NP.Seq 21 22
  , NP.Assign 22 "undefed" (NP.Num 0)
  , NP.Seq 22 23
  , NP.Assign 23 "undefed" (NP.Num 0)
  , NP.Seq 23 24
  , NP.Assign 24 "undefed" (NP.Num 0)
  , NP.Seq 24 25
  , NP.Cond 25 (NP.Eq (NP.Id "throwflag") (NP.Num 1)) 27 33
  , NP.Cond 27 (NP.Eq (NP.Num 0) (NP.Num 1)) 29 29
  , NP.Seq 28 262
  , NP.Seq 28 30
  , NP.Var 29 "NOP_29"
  , NP.Seq 29 30
  , NP.Var 30 "IF_ELSE_FOOTER"
  , NP.Assign 31 "next_instr" (NP.Num 0)
  , NP.Seq 31 32
  , NP.Assign 32 "stack_pointer" (NP.Num 0)
  , NP.Seq 32 33
  , NP.Seq 32 177
  , NP.Seq 32 34
  , NP.Var 33 "NOP_33"
  , NP.Seq 33 34
  , NP.Var 34 "IF_ELSE_FOOTER"
  , NP.Seq 34 246
  , NP.Var 35 "__CLABEL_dispatch_opcode"
  , NP.Seq 35 36
  , NP.Cond 36 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 38 87
  , NP.Var 38 "NOP_38"
  , NP.Var 39 "__CLABEL_TARGET_BINARY_OP"
  , NP.Seq 39 40
  , NP.Assign 40 "undefed" (NP.Num 0)
  , NP.Seq 40 41
  , NP.Assign 41 "next_instr" (NP.Num 0)
  , NP.Seq 41 42
  , NP.Var 42 "__CLABEL_PREDICTED_BINARY_OP"
  , NP.Seq 42 43
  , NP.Var 43 "NOP_43"
  , NP.Var 44 "this_instr"
  , NP.Seq 44 45
  , NP.Var 45 "lhs"
  , NP.Seq 45 46
  , NP.Var 46 "rhs"
  , NP.Seq 46 47
  , NP.Var 47 "res"
  , NP.Seq 47 48
  , NP.Assign 48 "rhs" (NP.Num 0)
  , NP.Seq 48 49
  , NP.Assign 49 "lhs" (NP.Num 0)
  , NP.Seq 49 50
  , NP.Var 50 "counter"
  , NP.Seq 50 51
  , NP.Cond 51 (NP.Eq (NP.Num 0) (NP.Num 1)) 53 56
  , NP.Assign 53 "next_instr" (NP.Num 0)
  , NP.Seq 53 54
  , NP.Assign 54 "stack_pointer" (NP.Num 0)
  , NP.Seq 54 55
  , NP.Assign 55 "opcode" (NP.Num 0)
  , NP.Seq 55 56
  , NP.Seq 55 35
  , NP.Seq 55 57
  , NP.Var 56 "NOP_56"
  , NP.Seq 56 57
  , NP.Var 57 "IF_ELSE_FOOTER"
  , NP.Assign 58 "undefed" (NP.Num 0)
  , NP.Seq 58 59
  , NP.Cond 59 (NP.Eq (NP.Num 0) (NP.Num 1)) 60 61
  , NP.Assign 60 "undefed" (NP.Num 0)
  , NP.Seq 60 59
  , NP.Var 61 "LOOP_FOOTER"
  , NP.Seq 61 62
  , NP.Var 62 "lhs_o"
  , NP.Seq 62 63
  , NP.Var 63 "rhs_o"
  , NP.Seq 63 64
  , NP.Var 64 "res_o"
  , NP.Seq 64 65
  , NP.Assign 65 "stack_pointer" (NP.Num 0)
  , NP.Seq 65 66
  , NP.Cond 66 (NP.Eq (NP.Plus (NP.Id "res_o") (NP.Num 0)) (NP.Num 1)) 68 68
  , NP.Seq 67 177
  , NP.Seq 67 69
  , NP.Var 68 "NOP_68"
  , NP.Seq 68 69
  , NP.Var 69 "IF_ELSE_FOOTER"
  , NP.Assign 70 "res" (NP.Num 0)
  , NP.Seq 70 71
  , NP.Var 71 "tmp"
  , NP.Seq 71 72
  , NP.Assign 72 "lhs" (NP.Num 0)
  , NP.Seq 72 73
  , NP.Assign 73 "undefed" (NP.Num 0)
  , NP.Seq 73 74
  , NP.Assign 74 "tmp" (NP.Num 0)
  , NP.Seq 74 75
  , NP.Assign 75 "rhs" (NP.Num 0)
  , NP.Seq 75 76
  , NP.Assign 76 "undefed" (NP.Num 0)
  , NP.Seq 76 77
  , NP.Assign 77 "stack_pointer" (NP.Num 0)
  , NP.Seq 77 78
  , NP.Assign 78 "stack_pointer" (NP.Num 0)
  , NP.Seq 78 79
  , NP.Var 79 "word"
  , NP.Seq 79 80
  , NP.Assign 80 "opcode" (NP.Num 0)
  , NP.Seq 80 81
  , NP.Assign 81 "oparg" (NP.Num 0)
  , NP.Seq 81 82
  , NP.Cond 82 (NP.Eq (NP.Num 0) (NP.Num 1)) 83 86
  , NP.Var 83 "word"
  , NP.Seq 83 84
  , NP.Assign 84 "opcode" (NP.Num 0)
  , NP.Seq 84 85
  , NP.Assign 85 "oparg" (NP.Num 0)
  , NP.Seq 85 86
  , NP.Seq 85 82
  , NP.Var 86 "LOOP_FOOTER"
  , NP.Seq 86 87
  , NP.Seq 86 35
  , NP.Cond 87 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 89 129
  , NP.Var 89 "NOP_89"
  , NP.Var 90 "__CLABEL_TARGET_BINARY_OP_ADD_FLOAT"
  , NP.Seq 90 91
  , NP.Var 91 "this_instr"
  , NP.Seq 91 92
  , NP.Assign 92 "undefed" (NP.Num 0)
  , NP.Seq 92 93
  , NP.Assign 93 "next_instr" (NP.Num 0)
  , NP.Seq 93 94
  , NP.Var 94 "value"
  , NP.Seq 94 95
  , NP.Var 95 "left"
  , NP.Seq 95 96
  , NP.Var 96 "right"
  , NP.Seq 96 97
  , NP.Var 97 "res"
  , NP.Seq 97 98
  , NP.Assign 98 "value" (NP.Num 0)
  , NP.Seq 98 99
  , NP.Var 99 "value_o"
  , NP.Seq 99 100
  , NP.Cond 100 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Plus (NP.Id "PyFloat_Type") (NP.Num 0))) (NP.Num 0)) (NP.Num 1)) 102 102
  , NP.Seq 101 42
  , NP.Seq 101 103
  , NP.Var 102 "NOP_102"
  , NP.Seq 102 103
  , NP.Var 103 "IF_ELSE_FOOTER"
  , NP.Assign 104 "left" (NP.Num 0)
  , NP.Seq 104 105
  , NP.Var 105 "left_o"
  , NP.Seq 105 106
  , NP.Cond 106 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Plus (NP.Id "PyFloat_Type") (NP.Num 0))) (NP.Num 0)) (NP.Num 1)) 108 108
  , NP.Seq 107 42
  , NP.Seq 107 109
  , NP.Var 108 "NOP_108"
  , NP.Seq 108 109
  , NP.Var 109 "IF_ELSE_FOOTER"
  , NP.Assign 110 "right" (NP.Num 0)
  , NP.Seq 110 111
  , NP.Var 111 "left_o"
  , NP.Seq 111 112
  , NP.Var 112 "right_o"
  , NP.Seq 112 113
  , NP.Var 113 "dres"
  , NP.Seq 113 114
  , NP.Assign 114 "res" (NP.Num 0)
  , NP.Seq 114 115
  , NP.Cond 115 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 117 117
  , NP.Seq 116 173
  , NP.Seq 116 118
  , NP.Var 117 "NOP_117"
  , NP.Seq 117 118
  , NP.Var 118 "IF_ELSE_FOOTER"
  , NP.Assign 119 "undefed" (NP.Num 0)
  , NP.Seq 119 120
  , NP.Assign 120 "stack_pointer" (NP.Num 0)
  , NP.Seq 120 121
  , NP.Var 121 "word"
  , NP.Seq 121 122
  , NP.Assign 122 "opcode" (NP.Num 0)
  , NP.Seq 122 123
  , NP.Assign 123 "oparg" (NP.Num 0)
  , NP.Seq 123 124
  , NP.Cond 124 (NP.Eq (NP.Num 0) (NP.Num 1)) 125 128
  , NP.Var 125 "word"
  , NP.Seq 125 126
  , NP.Assign 126 "opcode" (NP.Num 0)
  , NP.Seq 126 127
  , NP.Assign 127 "oparg" (NP.Num 0)
  , NP.Seq 127 128
  , NP.Seq 127 124
  , NP.Var 128 "LOOP_FOOTER"
  , NP.Seq 128 129
  , NP.Seq 128 35
  , NP.Cond 129 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 131 170
  , NP.Var 131 "NOP_131"
  , NP.Var 132 "__CLABEL_TARGET_BINARY_OP_ADD_INT"
  , NP.Seq 132 133
  , NP.Var 133 "this_instr"
  , NP.Seq 133 134
  , NP.Assign 134 "undefed" (NP.Num 0)
  , NP.Seq 134 135
  , NP.Assign 135 "next_instr" (NP.Num 0)
  , NP.Seq 135 136
  , NP.Var 136 "value"
  , NP.Seq 136 137
  , NP.Var 137 "left"
  , NP.Seq 137 138
  , NP.Var 138 "right"
  , NP.Seq 138 139
  , NP.Var 139 "res"
  , NP.Seq 139 140
  , NP.Assign 140 "value" (NP.Num 0)
  , NP.Seq 140 141
  , NP.Var 141 "value_o"
  , NP.Seq 141 142
  , NP.Cond 142 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 144 144
  , NP.Seq 143 42
  , NP.Seq 143 145
  , NP.Var 144 "NOP_144"
  , NP.Seq 144 145
  , NP.Var 145 "IF_ELSE_FOOTER"
  , NP.Assign 146 "left" (NP.Num 0)
  , NP.Seq 146 147
  , NP.Var 147 "left_o"
  , NP.Seq 147 148
  , NP.Cond 148 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 150 150
  , NP.Seq 149 42
  , NP.Seq 149 151
  , NP.Var 150 "NOP_150"
  , NP.Seq 150 151
  , NP.Var 151 "IF_ELSE_FOOTER"
  , NP.Assign 152 "right" (NP.Num 0)
  , NP.Seq 152 153
  , NP.Var 153 "left_o"
  , NP.Seq 153 154
  , NP.Var 154 "right_o"
  , NP.Seq 154 155
  , NP.Assign 155 "res" (NP.Num 0)
  , NP.Seq 155 156
  , NP.Cond 156 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 158 158
  , NP.Seq 157 42
  , NP.Seq 157 159
  , NP.Var 158 "NOP_158"
  , NP.Seq 158 159
  , NP.Var 159 "IF_ELSE_FOOTER"
  , NP.Assign 160 "undefed" (NP.Num 0)
  , NP.Seq 160 161
  , NP.Assign 161 "stack_pointer" (NP.Num 0)
  , NP.Seq 161 162
  , NP.Var 162 "word"
  , NP.Seq 162 163
  , NP.Assign 163 "opcode" (NP.Num 0)
  , NP.Seq 163 164
  , NP.Assign 164 "oparg" (NP.Num 0)
  , NP.Seq 164 165
  , NP.Cond 165 (NP.Eq (NP.Num 0) (NP.Num 1)) 166 169
  , NP.Var 166 "word"
  , NP.Seq 166 167
  , NP.Assign 167 "opcode" (NP.Num 0)
  , NP.Seq 167 168
  , NP.Assign 168 "oparg" (NP.Num 0)
  , NP.Seq 168 169
  , NP.Seq 168 165
  , NP.Var 169 "LOOP_FOOTER"
  , NP.Seq 169 170
  , NP.Seq 169 35
  , NP.Var 170 "NOP_170"
  , NP.Seq 170 171
  , NP.Var 171 "__CLABEL_CODEGEN_SWITCH_EXIT_0"
  , NP.Seq 171 172
  , NP.Var 172 "NOP_172"
  , NP.Var 173 "__CLABEL_pop_2_error"
  , NP.Seq 173 174
  , NP.Assign 174 "stack_pointer" (NP.Num 0)
  , NP.Seq 174 175
  , NP.Seq 174 177
  , NP.Var 175 "__CLABEL_pop_1_error"
  , NP.Seq 175 176
  , NP.Assign 176 "stack_pointer" (NP.Num 0)
  , NP.Seq 176 177
  , NP.Seq 176 177
  , NP.Var 177 "__CLABEL_error"
  , NP.Seq 177 178
  , NP.Cond 178 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 180 181
  , NP.Assign 180 "stack_pointer" (NP.Num 0)
  , NP.Seq 180 182
  , NP.Var 181 "NOP_181"
  , NP.Seq 181 182
  , NP.Var 182 "IF_ELSE_FOOTER"
  , NP.Cond 183 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 185 192
  , NP.Var 185 "f"
  , NP.Seq 185 186
  , NP.Assign 186 "stack_pointer" (NP.Num 0)
  , NP.Seq 186 187
  , NP.Cond 187 (NP.Eq (NP.Plus (NP.Id "f") (NP.Num 0)) (NP.Num 1)) 189 190
  , NP.Assign 189 "stack_pointer" (NP.Num 0)
  , NP.Seq 189 191
  , NP.Var 190 "NOP_190"
  , NP.Seq 190 191
  , NP.Var 191 "IF_ELSE_FOOTER"
  , NP.Seq 191 193
  , NP.Var 192 "NOP_192"
  , NP.Seq 192 193
  , NP.Var 193 "IF_ELSE_FOOTER"
  , NP.Seq 193 194
  , NP.Var 194 "__CLABEL_exception_unwind"
  , NP.Seq 194 195
  , NP.Var 195 "offset"
  , NP.Seq 195 196
  , NP.Var 196 "level"
  , NP.Seq 196 197
  , NP.Var 197 "handler"
  , NP.Seq 197 198
  , NP.Var 198 "lasti"
  , NP.Seq 198 199
  , NP.Var 199 "handled"
  , NP.Seq 199 200
  , NP.Cond 200 (NP.Eq (NP.Plus (NP.Id "handled") (NP.Num 0)) (NP.Num 1)) 202 206
  , NP.Var 202 "stackbase"
  , NP.Seq 202 203
  , NP.Cond 203 (NP.Eq (NP.Plus (NP.Num 0) (NP.Id "stackbase")) (NP.Num 1)) 204 205
  , NP.Var 204 "ref"
  , NP.Seq 204 205
  , NP.Seq 204 203
  , NP.Var 205 "LOOP_FOOTER"
  , NP.Seq 205 206
  , NP.Seq 205 234
  , NP.Seq 205 207
  , NP.Var 206 "NOP_206"
  , NP.Seq 206 207
  , NP.Var 207 "IF_ELSE_FOOTER"
  , NP.Var 208 "new_top"
  , NP.Seq 208 209
  , NP.Cond 209 (NP.Eq (NP.Plus (NP.Num 0) (NP.Id "new_top")) (NP.Num 1)) 210 211
  , NP.Var 210 "ref"
  , NP.Seq 210 211
  , NP.Seq 210 209
  , NP.Var 211 "LOOP_FOOTER"
  , NP.Seq 211 212
  , NP.Cond 212 (NP.Eq (NP.Id "lasti") (NP.Num 1)) 214 216
  , NP.Var 214 "frame_lasti"
  , NP.Seq 214 215
  , NP.Var 215 "lasti"
  , NP.Seq 215 216
  , NP.Seq 215 217
  , NP.Var 216 "NOP_216"
  , NP.Seq 216 217
  , NP.Var 217 "IF_ELSE_FOOTER"
  , NP.Var 218 "exc"
  , NP.Seq 218 219
  , NP.Assign 219 "next_instr" (NP.Num 0)
  , NP.Seq 219 220
  , NP.Var 220 "err"
  , NP.Seq 220 221
  , NP.Cond 221 (NP.Eq (NP.Plus (NP.Id "err") (NP.Num 0)) (NP.Num 1)) 223 223
  , NP.Seq 222 194
  , NP.Seq 222 224
  , NP.Var 223 "NOP_223"
  , NP.Seq 223 224
  , NP.Var 224 "IF_ELSE_FOOTER"
  , NP.Assign 225 "stack_pointer" (NP.Num 0)
  , NP.Seq 225 226
  , NP.Var 226 "word"
  , NP.Seq 226 227
  , NP.Assign 227 "opcode" (NP.Num 0)
  , NP.Seq 227 228
  , NP.Assign 228 "oparg" (NP.Num 0)
  , NP.Seq 228 229
  , NP.Cond 229 (NP.Eq (NP.Num 0) (NP.Num 1)) 230 233
  , NP.Var 230 "word"
  , NP.Seq 230 231
  , NP.Assign 231 "opcode" (NP.Num 0)
  , NP.Seq 231 232
  , NP.Assign 232 "oparg" (NP.Num 0)
  , NP.Seq 232 233
  , NP.Seq 232 229
  , NP.Var 233 "LOOP_FOOTER"
  , NP.Seq 233 234
  , NP.Seq 233 35
  , NP.Var 234 "__CLABEL_exit_unwind"
  , NP.Seq 234 235
  , NP.Var 235 "dying"
  , NP.Seq 235 236
  , NP.Assign 236 "frame" (NP.Num 0)
  , NP.Seq 236 237
  , NP.Assign 237 "undefed" (NP.Num 0)
  , NP.Seq 237 238
  , NP.Cond 238 (NP.Eq (NP.Plus (NP.Num 0) (NP.Id "FRAME_OWNED_BY_INTERPRETER")) (NP.Num 1)) 240 242
  , NP.Assign 240 "undefed" (NP.Num 0)
  , NP.Seq 240 241
  , NP.Assign 241 "return" (NP.Num 0)
  , NP.Seq 241 242
  , NP.Seq 241 243
  , NP.Var 242 "NOP_242"
  , NP.Seq 242 243
  , NP.Var 243 "IF_ELSE_FOOTER"
  , NP.Assign 244 "next_instr" (NP.Num 0)
  , NP.Seq 244 245
  , NP.Assign 245 "stack_pointer" (NP.Num 0)
  , NP.Seq 245 246
  , NP.Seq 245 177
  , NP.Var 246 "__CLABEL_start_frame"
  , NP.Seq 246 247
  , NP.Var 247 "too_deep"
  , NP.Seq 247 248
  , NP.Cond 248 (NP.Eq (NP.Id "too_deep") (NP.Num 1)) 250 250
  , NP.Seq 249 234
  , NP.Seq 249 251
  , NP.Var 250 "NOP_250"
  , NP.Seq 250 251
  , NP.Var 251 "IF_ELSE_FOOTER"
  , NP.Assign 252 "next_instr" (NP.Num 0)
  , NP.Seq 252 253
  , NP.Assign 253 "stack_pointer" (NP.Num 0)
  , NP.Seq 253 254
  , NP.Var 254 "word"
  , NP.Seq 254 255
  , NP.Assign 255 "opcode" (NP.Num 0)
  , NP.Seq 255 256
  , NP.Assign 256 "oparg" (NP.Num 0)
  , NP.Seq 256 257
  , NP.Cond 257 (NP.Eq (NP.Num 0) (NP.Num 1)) 258 261
  , NP.Var 258 "word"
  , NP.Seq 258 259
  , NP.Assign 259 "opcode" (NP.Num 0)
  , NP.Seq 259 260
  , NP.Assign 260 "oparg" (NP.Num 0)
  , NP.Seq 260 261
  , NP.Seq 260 257
  , NP.Var 261 "LOOP_FOOTER"
  , NP.Seq 261 262
  , NP.Seq 261 35
  , NP.Var 262 "__CLABEL_early_exit"
  , NP.Seq 262 263
  , NP.Var 263 "NOP_263"
  , NP.Var 264 "dying"
  , NP.Seq 264 265
  , NP.Assign 265 "frame" (NP.Num 0)
  , NP.Seq 265 266
  , NP.Assign 266 "undefed" (NP.Num 0)
  , NP.Seq 266 267
  , NP.Assign 267 "undefed" (NP.Num 0)
  , NP.Seq 267 268
  , NP.Assign 268 "return" (NP.Num 0)
  , NP.Seq 268 269
  , NP.Seq 268 269
  , NP.Var 269 "PROG_END"
  ]

withPrioritiesTest :: [WP.Fact]
withPrioritiesTest =
  [ WP.Var 0 "_PyEval_EvalFrameDefault"
  , WP.Seq 0 1
  , WP.Var 1 "tstate"
  , WP.Seq 1 2
  , WP.Var 2 "frame"
  , WP.Seq 2 3
  , WP.Var 3 "throwflag"
  , WP.Seq 3 4
  , WP.Var 4 "opcode"
  , WP.Seq 4 5
  , WP.Var 5 "oparg"
  , WP.Seq 5 6
  , WP.Var 6 "entry"
  , WP.Seq 6 7
  , WP.Cond 7 (WP.Eq (WP.Num 0) (WP.Num 1)) 9 10
  , WP.Assign 9 "return" (WP.Num 0)
  , WP.Seq 9 11
  , WP.Var 10 "NOP_10"
  , WP.Seq 10 11
  , WP.Var 11 "IF_ELSE_FOOTER"
  , WP.Var 12 "next_instr"
  , WP.Seq 12 13
  , WP.Var 13 "stack_pointer"
  , WP.Seq 13 14
  , WP.Assign 14 "undefed" (WP.Num 0)
  , WP.Seq 14 15
  , WP.Assign 15 "undefed" (WP.Num 0)
  , WP.Seq 15 16
  , WP.Assign 16 "undefed" (WP.Num 0)
  , WP.Seq 16 17
  , WP.Assign 17 "undefed" (WP.Num 0)
  , WP.Seq 17 18
  , WP.Assign 18 "undefed" (WP.Num 0)
  , WP.Seq 18 19
  , WP.Assign 19 "undefed" (WP.Num 0)
  , WP.Seq 19 20
  , WP.Assign 20 "undefed" (WP.Num 0)
  , WP.Seq 20 21
  , WP.Assign 21 "undefed" (WP.Num 0)
  , WP.Seq 21 22
  , WP.Assign 22 "undefed" (WP.Num 0)
  , WP.Seq 22 23
  , WP.Assign 23 "undefed" (WP.Num 0)
  , WP.Seq 23 24
  , WP.Assign 24 "undefed" (WP.Num 0)
  , WP.Seq 24 25
  , WP.Cond 25 (WP.Eq (WP.Id "throwflag") (WP.Num 1)) 27 33
  , WP.Cond 27 (WP.Eq (WP.Num 0) (WP.Num 1)) 29 29
  , WP.Seq 28 262
  , WP.Seq 28 30
  , WP.Var 29 "NOP_29"
  , WP.Seq 29 30
  , WP.Var 30 "IF_ELSE_FOOTER"
  , WP.Assign 31 "next_instr" (WP.Num 0)
  , WP.Seq 31 32
  , WP.Assign 32 "stack_pointer" (WP.Num 0)
  , WP.Seq 32 33
  , WP.Seq 32 177
  , WP.Seq 32 34
  , WP.Var 33 "NOP_33"
  , WP.Seq 33 34
  , WP.Var 34 "IF_ELSE_FOOTER"
  , WP.Seq 34 246
  , WP.Var 35 "__CLABEL_dispatch_opcode"
  , WP.Seq 35 36
  , WP.Cond 36 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 38 87
  , WP.Var 38 "NOP_38"
  , WP.Var 39 "__CLABEL_TARGET_BINARY_OP"
  , WP.Seq 39 40
  , WP.Assign 40 "undefed" (WP.Num 0)
  , WP.Seq 40 41
  , WP.Assign 41 "next_instr" (WP.Num 0)
  , WP.Seq 41 42
  , WP.Var 42 "__CLABEL_PREDICTED_BINARY_OP"
  , WP.Seq 42 43
  , WP.Var 43 "NOP_43"
  , WP.Var 44 "this_instr"
  , WP.Seq 44 45
  , WP.Var 45 "lhs"
  , WP.Seq 45 46
  , WP.Var 46 "rhs"
  , WP.Seq 46 47
  , WP.Var 47 "res"
  , WP.Seq 47 48
  , WP.Assign 48 "rhs" (WP.Num 0)
  , WP.Seq 48 49
  , WP.Assign 49 "lhs" (WP.Num 0)
  , WP.Seq 49 50
  , WP.Var 50 "counter"
  , WP.Seq 50 51
  , WP.Cond 51 (WP.Eq (WP.Num 0) (WP.Num 1)) 53 56
  , WP.Assign 53 "next_instr" (WP.Num 0)
  , WP.Seq 53 54
  , WP.Assign 54 "stack_pointer" (WP.Num 0)
  , WP.Seq 54 55
  , WP.Assign 55 "opcode" (WP.Num 0)
  , WP.Seq 55 56
  , WP.Seq 55 35
  , WP.Seq 55 57
  , WP.Var 56 "NOP_56"
  , WP.Seq 56 57
  , WP.Var 57 "IF_ELSE_FOOTER"
  , WP.Assign 58 "undefed" (WP.Num 0)
  , WP.Seq 58 59
  , WP.Cond 59 (WP.Eq (WP.Num 0) (WP.Num 1)) 60 61
  , WP.Assign 60 "undefed" (WP.Num 0)
  , WP.Seq 60 59
  , WP.Var 61 "LOOP_FOOTER"
  , WP.Seq 61 62
  , WP.Var 62 "lhs_o"
  , WP.Seq 62 63
  , WP.Var 63 "rhs_o"
  , WP.Seq 63 64
  , WP.Var 64 "res_o"
  , WP.Seq 64 65
  , WP.Assign 65 "stack_pointer" (WP.Num 0)
  , WP.Seq 65 66
  , WP.Cond 66 (WP.Eq (WP.Plus (WP.Id "res_o") (WP.Num 0)) (WP.Num 1)) 68 68
  , WP.Seq 67 177
  , WP.Seq 67 69
  , WP.Var 68 "NOP_68"
  , WP.Seq 68 69
  , WP.Var 69 "IF_ELSE_FOOTER"
  , WP.Assign 70 "res" (WP.Num 0)
  , WP.Seq 70 71
  , WP.Var 71 "tmp"
  , WP.Seq 71 72
  , WP.Assign 72 "lhs" (WP.Num 0)
  , WP.Seq 72 73
  , WP.Assign 73 "undefed" (WP.Num 0)
  , WP.Seq 73 74
  , WP.Assign 74 "tmp" (WP.Num 0)
  , WP.Seq 74 75
  , WP.Assign 75 "rhs" (WP.Num 0)
  , WP.Seq 75 76
  , WP.Assign 76 "undefed" (WP.Num 0)
  , WP.Seq 76 77
  , WP.Assign 77 "stack_pointer" (WP.Num 0)
  , WP.Seq 77 78
  , WP.Assign 78 "stack_pointer" (WP.Num 0)
  , WP.Seq 78 79
  , WP.Var 79 "word"
  , WP.Seq 79 80
  , WP.Assign 80 "opcode" (WP.Num 0)
  , WP.Seq 80 81
  , WP.Assign 81 "oparg" (WP.Num 0)
  , WP.Seq 81 82
  , WP.Cond 82 (WP.Eq (WP.Num 0) (WP.Num 1)) 83 86
  , WP.Var 83 "word"
  , WP.Seq 83 84
  , WP.Assign 84 "opcode" (WP.Num 0)
  , WP.Seq 84 85
  , WP.Assign 85 "oparg" (WP.Num 0)
  , WP.Seq 85 86
  , WP.Seq 85 82
  , WP.Var 86 "LOOP_FOOTER"
  , WP.Seq 86 87
  , WP.Seq 86 35
  , WP.Cond 87 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 89 129
  , WP.Var 89 "NOP_89"
  , WP.Var 90 "__CLABEL_TARGET_BINARY_OP_ADD_FLOAT"
  , WP.Seq 90 91
  , WP.Var 91 "this_instr"
  , WP.Seq 91 92
  , WP.Assign 92 "undefed" (WP.Num 0)
  , WP.Seq 92 93
  , WP.Assign 93 "next_instr" (WP.Num 0)
  , WP.Seq 93 94
  , WP.Var 94 "value"
  , WP.Seq 94 95
  , WP.Var 95 "left"
  , WP.Seq 95 96
  , WP.Var 96 "right"
  , WP.Seq 96 97
  , WP.Var 97 "res"
  , WP.Seq 97 98
  , WP.Assign 98 "value" (WP.Num 0)
  , WP.Seq 98 99
  , WP.Var 99 "value_o"
  , WP.Seq 99 100
  , WP.Cond 100 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Plus (WP.Id "PyFloat_Type") (WP.Num 0))) (WP.Num 0)) (WP.Num 1)) 102 102
  , WP.Seq 101 42
  , WP.Seq 101 103
  , WP.Var 102 "NOP_102"
  , WP.Seq 102 103
  , WP.Var 103 "IF_ELSE_FOOTER"
  , WP.Assign 104 "left" (WP.Num 0)
  , WP.Seq 104 105
  , WP.Var 105 "left_o"
  , WP.Seq 105 106
  , WP.Cond 106 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Plus (WP.Id "PyFloat_Type") (WP.Num 0))) (WP.Num 0)) (WP.Num 1)) 108 108
  , WP.Seq 107 42
  , WP.Seq 107 109
  , WP.Var 108 "NOP_108"
  , WP.Seq 108 109
  , WP.Var 109 "IF_ELSE_FOOTER"
  , WP.Assign 110 "right" (WP.Num 0)
  , WP.Seq 110 111
  , WP.Var 111 "left_o"
  , WP.Seq 111 112
  , WP.Var 112 "right_o"
  , WP.Seq 112 113
  , WP.Var 113 "dres"
  , WP.Seq 113 114
  , WP.Assign 114 "res" (WP.Num 0)
  , WP.Seq 114 115
  , WP.Cond 115 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 117 117
  , WP.Seq 116 173
  , WP.Seq 116 118
  , WP.Var 117 "NOP_117"
  , WP.Seq 117 118
  , WP.Var 118 "IF_ELSE_FOOTER"
  , WP.Assign 119 "undefed" (WP.Num 0)
  , WP.Seq 119 120
  , WP.Assign 120 "stack_pointer" (WP.Num 0)
  , WP.Seq 120 121
  , WP.Var 121 "word"
  , WP.Seq 121 122
  , WP.Assign 122 "opcode" (WP.Num 0)
  , WP.Seq 122 123
  , WP.Assign 123 "oparg" (WP.Num 0)
  , WP.Seq 123 124
  , WP.Cond 124 (WP.Eq (WP.Num 0) (WP.Num 1)) 125 128
  , WP.Var 125 "word"
  , WP.Seq 125 126
  , WP.Assign 126 "opcode" (WP.Num 0)
  , WP.Seq 126 127
  , WP.Assign 127 "oparg" (WP.Num 0)
  , WP.Seq 127 128
  , WP.Seq 127 124
  , WP.Var 128 "LOOP_FOOTER"
  , WP.Seq 128 129
  , WP.Seq 128 35
  , WP.Cond 129 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 131 170
  , WP.Var 131 "NOP_131"
  , WP.Var 132 "__CLABEL_TARGET_BINARY_OP_ADD_INT"
  , WP.Seq 132 133
  , WP.Var 133 "this_instr"
  , WP.Seq 133 134
  , WP.Assign 134 "undefed" (WP.Num 0)
  , WP.Seq 134 135
  , WP.Assign 135 "next_instr" (WP.Num 0)
  , WP.Seq 135 136
  , WP.Var 136 "value"
  , WP.Seq 136 137
  , WP.Var 137 "left"
  , WP.Seq 137 138
  , WP.Var 138 "right"
  , WP.Seq 138 139
  , WP.Var 139 "res"
  , WP.Seq 139 140
  , WP.Assign 140 "value" (WP.Num 0)
  , WP.Seq 140 141
  , WP.Var 141 "value_o"
  , WP.Seq 141 142
  , WP.Cond 142 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 144 144
  , WP.Seq 143 42
  , WP.Seq 143 145
  , WP.Var 144 "NOP_144"
  , WP.Seq 144 145
  , WP.Var 145 "IF_ELSE_FOOTER"
  , WP.Assign 146 "left" (WP.Num 0)
  , WP.Seq 146 147
  , WP.Var 147 "left_o"
  , WP.Seq 147 148
  , WP.Cond 148 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 150 150
  , WP.Seq 149 42
  , WP.Seq 149 151
  , WP.Var 150 "NOP_150"
  , WP.Seq 150 151
  , WP.Var 151 "IF_ELSE_FOOTER"
  , WP.Assign 152 "right" (WP.Num 0)
  , WP.Seq 152 153
  , WP.Var 153 "left_o"
  , WP.Seq 153 154
  , WP.Var 154 "right_o"
  , WP.Seq 154 155
  , WP.Assign 155 "res" (WP.Num 0)
  , WP.Seq 155 156
  , WP.Cond 156 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 158 158
  , WP.Seq 157 42
  , WP.Seq 157 159
  , WP.Var 158 "NOP_158"
  , WP.Seq 158 159
  , WP.Var 159 "IF_ELSE_FOOTER"
  , WP.Assign 160 "undefed" (WP.Num 0)
  , WP.Seq 160 161
  , WP.Assign 161 "stack_pointer" (WP.Num 0)
  , WP.Seq 161 162
  , WP.Var 162 "word"
  , WP.Seq 162 163
  , WP.Assign 163 "opcode" (WP.Num 0)
  , WP.Seq 163 164
  , WP.Assign 164 "oparg" (WP.Num 0)
  , WP.Seq 164 165
  , WP.Cond 165 (WP.Eq (WP.Num 0) (WP.Num 1)) 166 169
  , WP.Var 166 "word"
  , WP.Seq 166 167
  , WP.Assign 167 "opcode" (WP.Num 0)
  , WP.Seq 167 168
  , WP.Assign 168 "oparg" (WP.Num 0)
  , WP.Seq 168 169
  , WP.Seq 168 165
  , WP.Var 169 "LOOP_FOOTER"
  , WP.Seq 169 170
  , WP.Seq 169 35
  , WP.Var 170 "NOP_170"
  , WP.Seq 170 171
  , WP.Var 171 "__CLABEL_CODEGEN_SWITCH_EXIT_0"
  , WP.Seq 171 172
  , WP.Var 172 "NOP_172"
  , WP.Var 173 "__CLABEL_pop_2_error"
  , WP.Seq 173 174
  , WP.Assign 174 "stack_pointer" (WP.Num 0)
  , WP.Seq 174 175
  , WP.Seq 174 177
  , WP.Var 175 "__CLABEL_pop_1_error"
  , WP.Seq 175 176
  , WP.Assign 176 "stack_pointer" (WP.Num 0)
  , WP.Seq 176 177
  , WP.Seq 176 177
  , WP.Var 177 "__CLABEL_error"
  , WP.Seq 177 178
  , WP.Cond 178 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 180 181
  , WP.Assign 180 "stack_pointer" (WP.Num 0)
  , WP.Seq 180 182
  , WP.Var 181 "NOP_181"
  , WP.Seq 181 182
  , WP.Var 182 "IF_ELSE_FOOTER"
  , WP.Cond 183 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 185 192
  , WP.Var 185 "f"
  , WP.Seq 185 186
  , WP.Assign 186 "stack_pointer" (WP.Num 0)
  , WP.Seq 186 187
  , WP.Cond 187 (WP.Eq (WP.Plus (WP.Id "f") (WP.Num 0)) (WP.Num 1)) 189 190
  , WP.Assign 189 "stack_pointer" (WP.Num 0)
  , WP.Seq 189 191
  , WP.Var 190 "NOP_190"
  , WP.Seq 190 191
  , WP.Var 191 "IF_ELSE_FOOTER"
  , WP.Seq 191 193
  , WP.Var 192 "NOP_192"
  , WP.Seq 192 193
  , WP.Var 193 "IF_ELSE_FOOTER"
  , WP.Seq 193 194
  , WP.Var 194 "__CLABEL_exception_unwind"
  , WP.Seq 194 195
  , WP.Var 195 "offset"
  , WP.Seq 195 196
  , WP.Var 196 "level"
  , WP.Seq 196 197
  , WP.Var 197 "handler"
  , WP.Seq 197 198
  , WP.Var 198 "lasti"
  , WP.Seq 198 199
  , WP.Var 199 "handled"
  , WP.Seq 199 200
  , WP.Cond 200 (WP.Eq (WP.Plus (WP.Id "handled") (WP.Num 0)) (WP.Num 1)) 202 206
  , WP.Var 202 "stackbase"
  , WP.Seq 202 203
  , WP.Cond 203 (WP.Eq (WP.Plus (WP.Num 0) (WP.Id "stackbase")) (WP.Num 1)) 204 205
  , WP.Var 204 "ref"
  , WP.Seq 204 205
  , WP.Seq 204 203
  , WP.Var 205 "LOOP_FOOTER"
  , WP.Seq 205 206
  , WP.Seq 205 234
  , WP.Seq 205 207
  , WP.Var 206 "NOP_206"
  , WP.Seq 206 207
  , WP.Var 207 "IF_ELSE_FOOTER"
  , WP.Var 208 "new_top"
  , WP.Seq 208 209
  , WP.Cond 209 (WP.Eq (WP.Plus (WP.Num 0) (WP.Id "new_top")) (WP.Num 1)) 210 211
  , WP.Var 210 "ref"
  , WP.Seq 210 211
  , WP.Seq 210 209
  , WP.Var 211 "LOOP_FOOTER"
  , WP.Seq 211 212
  , WP.Cond 212 (WP.Eq (WP.Id "lasti") (WP.Num 1)) 214 216
  , WP.Var 214 "frame_lasti"
  , WP.Seq 214 215
  , WP.Var 215 "lasti"
  , WP.Seq 215 216
  , WP.Seq 215 217
  , WP.Var 216 "NOP_216"
  , WP.Seq 216 217
  , WP.Var 217 "IF_ELSE_FOOTER"
  , WP.Var 218 "exc"
  , WP.Seq 218 219
  , WP.Assign 219 "next_instr" (WP.Num 0)
  , WP.Seq 219 220
  , WP.Var 220 "err"
  , WP.Seq 220 221
  , WP.Cond 221 (WP.Eq (WP.Plus (WP.Id "err") (WP.Num 0)) (WP.Num 1)) 223 223
  , WP.Seq 222 194
  , WP.Seq 222 224
  , WP.Var 223 "NOP_223"
  , WP.Seq 223 224
  , WP.Var 224 "IF_ELSE_FOOTER"
  , WP.Assign 225 "stack_pointer" (WP.Num 0)
  , WP.Seq 225 226
  , WP.Var 226 "word"
  , WP.Seq 226 227
  , WP.Assign 227 "opcode" (WP.Num 0)
  , WP.Seq 227 228
  , WP.Assign 228 "oparg" (WP.Num 0)
  , WP.Seq 228 229
  , WP.Cond 229 (WP.Eq (WP.Num 0) (WP.Num 1)) 230 233
  , WP.Var 230 "word"
  , WP.Seq 230 231
  , WP.Assign 231 "opcode" (WP.Num 0)
  , WP.Seq 231 232
  , WP.Assign 232 "oparg" (WP.Num 0)
  , WP.Seq 232 233
  , WP.Seq 232 229
  , WP.Var 233 "LOOP_FOOTER"
  , WP.Seq 233 234
  , WP.Seq 233 35
  , WP.Var 234 "__CLABEL_exit_unwind"
  , WP.Seq 234 235
  , WP.Var 235 "dying"
  , WP.Seq 235 236
  , WP.Assign 236 "frame" (WP.Num 0)
  , WP.Seq 236 237
  , WP.Assign 237 "undefed" (WP.Num 0)
  , WP.Seq 237 238
  , WP.Cond 238 (WP.Eq (WP.Plus (WP.Num 0) (WP.Id "FRAME_OWNED_BY_INTERPRETER")) (WP.Num 1)) 240 242
  , WP.Assign 240 "undefed" (WP.Num 0)
  , WP.Seq 240 241
  , WP.Assign 241 "return" (WP.Num 0)
  , WP.Seq 241 242
  , WP.Seq 241 243
  , WP.Var 242 "NOP_242"
  , WP.Seq 242 243
  , WP.Var 243 "IF_ELSE_FOOTER"
  , WP.Assign 244 "next_instr" (WP.Num 0)
  , WP.Seq 244 245
  , WP.Assign 245 "stack_pointer" (WP.Num 0)
  , WP.Seq 245 246
  , WP.Seq 245 177
  , WP.Var 246 "__CLABEL_start_frame"
  , WP.Seq 246 247
  , WP.Var 247 "too_deep"
  , WP.Seq 247 248
  , WP.Cond 248 (WP.Eq (WP.Id "too_deep") (WP.Num 1)) 250 250
  , WP.Seq 249 234
  , WP.Seq 249 251
  , WP.Var 250 "NOP_250"
  , WP.Seq 250 251
  , WP.Var 251 "IF_ELSE_FOOTER"
  , WP.Assign 252 "next_instr" (WP.Num 0)
  , WP.Seq 252 253
  , WP.Assign 253 "stack_pointer" (WP.Num 0)
  , WP.Seq 253 254
  , WP.Var 254 "word"
  , WP.Seq 254 255
  , WP.Assign 255 "opcode" (WP.Num 0)
  , WP.Seq 255 256
  , WP.Assign 256 "oparg" (WP.Num 0)
  , WP.Seq 256 257
  , WP.Cond 257 (WP.Eq (WP.Num 0) (WP.Num 1)) 258 261
  , WP.Var 258 "word"
  , WP.Seq 258 259
  , WP.Assign 259 "opcode" (WP.Num 0)
  , WP.Seq 259 260
  , WP.Assign 260 "oparg" (WP.Num 0)
  , WP.Seq 260 261
  , WP.Seq 260 257
  , WP.Var 261 "LOOP_FOOTER"
  , WP.Seq 261 262
  , WP.Seq 261 35
  , WP.Var 262 "__CLABEL_early_exit"
  , WP.Seq 262 263
  , WP.Var 263 "NOP_263"
  , WP.Var 264 "dying"
  , WP.Seq 264 265
  , WP.Assign 265 "frame" (WP.Num 0)
  , WP.Seq 265 266
  , WP.Assign 266 "undefed" (WP.Num 0)
  , WP.Seq 266 267
  , WP.Assign 267 "undefed" (WP.Num 0)
  , WP.Seq 267 268
  , WP.Assign 268 "return" (WP.Num 0)
  , WP.Seq 268 269
  , WP.Seq 268 269
  , WP.Var 269 "PROG_END"
  ]
