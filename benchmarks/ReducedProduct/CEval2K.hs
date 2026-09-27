module ReducedProduct.CEval2K where

import ReducedProduct.Common
import ReducedProduct.Custom qualified as C
import ReducedProduct.CustomN qualified as CN
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
  , H.mkBranch 7 (Eq (Num 0) (Num 1)) 9 10
  , H.mkAssign 9 "return" (Num 0)
  , H.mkSeq 9 11
  , H.mkVar 10 "NOP_10"
  , H.mkSeq 10 11
  , H.mkVar 11 "IF_ELSE_FOOTER"
  , H.mkVar 12 "next_instr"
  , H.mkSeq 12 13
  , H.mkVar 13 "stack_pointer"
  , H.mkSeq 13 14
  , H.mkAssign 14 "undefed" (Num 0)
  , H.mkSeq 14 15
  , H.mkAssign 15 "undefed" (Num 0)
  , H.mkSeq 15 16
  , H.mkAssign 16 "undefed" (Num 0)
  , H.mkSeq 16 17
  , H.mkAssign 17 "undefed" (Num 0)
  , H.mkSeq 17 18
  , H.mkAssign 18 "undefed" (Num 0)
  , H.mkSeq 18 19
  , H.mkAssign 19 "undefed" (Num 0)
  , H.mkSeq 19 20
  , H.mkAssign 20 "undefed" (Num 0)
  , H.mkSeq 20 21
  , H.mkAssign 21 "undefed" (Num 0)
  , H.mkSeq 21 22
  , H.mkAssign 22 "undefed" (Num 0)
  , H.mkSeq 22 23
  , H.mkAssign 23 "undefed" (Num 0)
  , H.mkSeq 23 24
  , H.mkAssign 24 "undefed" (Num 0)
  , H.mkSeq 24 25
  , H.mkBranch 25 (Eq (Id "throwflag") (Num 1)) 27 33
  , H.mkBranch 27 (Eq (Num 0) (Num 1)) 29 29
  , H.mkSeq 28 1902
  , H.mkSeq 28 30
  , H.mkVar 29 "NOP_29"
  , H.mkSeq 29 30
  , H.mkVar 30 "IF_ELSE_FOOTER"
  , H.mkAssign 31 "next_instr" (Num 0)
  , H.mkSeq 31 32
  , H.mkAssign 32 "stack_pointer" (Num 0)
  , H.mkSeq 32 33
  , H.mkSeq 32 1817
  , H.mkSeq 32 34
  , H.mkVar 33 "NOP_33"
  , H.mkSeq 33 34
  , H.mkVar 34 "IF_ELSE_FOOTER"
  , H.mkSeq 34 1886
  , H.mkVar 35 "__CLABEL_dispatch_opcode"
  , H.mkSeq 35 36
  , H.mkBranch 36 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 38 87
  , H.mkVar 38 "NOP_38"
  , H.mkVar 39 "__CLABEL_TARGET_BINARY_OP"
  , H.mkSeq 39 40
  , H.mkAssign 40 "undefed" (Num 0)
  , H.mkSeq 40 41
  , H.mkAssign 41 "next_instr" (Num 0)
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
  , H.mkAssign 48 "rhs" (Num 0)
  , H.mkSeq 48 49
  , H.mkAssign 49 "lhs" (Num 0)
  , H.mkSeq 49 50
  , H.mkVar 50 "counter"
  , H.mkSeq 50 51
  , H.mkBranch 51 (Eq (Num 0) (Num 1)) 53 56
  , H.mkAssign 53 "next_instr" (Num 0)
  , H.mkSeq 53 54
  , H.mkAssign 54 "stack_pointer" (Num 0)
  , H.mkSeq 54 55
  , H.mkAssign 55 "opcode" (Num 0)
  , H.mkSeq 55 56
  , H.mkSeq 55 35
  , H.mkSeq 55 57
  , H.mkVar 56 "NOP_56"
  , H.mkSeq 56 57
  , H.mkVar 57 "IF_ELSE_FOOTER"
  , H.mkAssign 58 "undefed" (Num 0)
  , H.mkSeq 58 59
  , H.mkBranch 59 (Eq (Num 0) (Num 1)) 60 61
  , H.mkAssign 60 "undefed" (Num 0)
  , H.mkSeq 60 59
  , H.mkVar 61 "LOOP_FOOTER"
  , H.mkSeq 61 62
  , H.mkVar 62 "lhs_o"
  , H.mkSeq 62 63
  , H.mkVar 63 "rhs_o"
  , H.mkSeq 63 64
  , H.mkVar 64 "res_o"
  , H.mkSeq 64 65
  , H.mkAssign 65 "stack_pointer" (Num 0)
  , H.mkSeq 65 66
  , H.mkBranch 66 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 68 68
  , H.mkSeq 67 1817
  , H.mkSeq 67 69
  , H.mkVar 68 "NOP_68"
  , H.mkSeq 68 69
  , H.mkVar 69 "IF_ELSE_FOOTER"
  , H.mkAssign 70 "res" (Num 0)
  , H.mkSeq 70 71
  , H.mkVar 71 "tmp"
  , H.mkSeq 71 72
  , H.mkAssign 72 "lhs" (Num 0)
  , H.mkSeq 72 73
  , H.mkAssign 73 "undefed" (Num 0)
  , H.mkSeq 73 74
  , H.mkAssign 74 "tmp" (Num 0)
  , H.mkSeq 74 75
  , H.mkAssign 75 "rhs" (Num 0)
  , H.mkSeq 75 76
  , H.mkAssign 76 "undefed" (Num 0)
  , H.mkSeq 76 77
  , H.mkAssign 77 "stack_pointer" (Num 0)
  , H.mkSeq 77 78
  , H.mkAssign 78 "stack_pointer" (Num 0)
  , H.mkSeq 78 79
  , H.mkVar 79 "word"
  , H.mkSeq 79 80
  , H.mkAssign 80 "opcode" (Num 0)
  , H.mkSeq 80 81
  , H.mkAssign 81 "oparg" (Num 0)
  , H.mkSeq 81 82
  , H.mkBranch 82 (Eq (Num 0) (Num 1)) 83 86
  , H.mkVar 83 "word"
  , H.mkSeq 83 84
  , H.mkAssign 84 "opcode" (Num 0)
  , H.mkSeq 84 85
  , H.mkAssign 85 "oparg" (Num 0)
  , H.mkSeq 85 86
  , H.mkSeq 85 82
  , H.mkVar 86 "LOOP_FOOTER"
  , H.mkSeq 86 87
  , H.mkSeq 86 35
  , H.mkBranch 87 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 89 129
  , H.mkVar 89 "NOP_89"
  , H.mkVar 90 "__CLABEL_TARGET_BINARY_OP_ADD_FLOAT"
  , H.mkSeq 90 91
  , H.mkVar 91 "this_instr"
  , H.mkSeq 91 92
  , H.mkAssign 92 "undefed" (Num 0)
  , H.mkSeq 92 93
  , H.mkAssign 93 "next_instr" (Num 0)
  , H.mkSeq 93 94
  , H.mkVar 94 "value"
  , H.mkSeq 94 95
  , H.mkVar 95 "left"
  , H.mkSeq 95 96
  , H.mkVar 96 "right"
  , H.mkSeq 96 97
  , H.mkVar 97 "res"
  , H.mkSeq 97 98
  , H.mkAssign 98 "value" (Num 0)
  , H.mkSeq 98 99
  , H.mkVar 99 "value_o"
  , H.mkSeq 99 100
  , H.mkBranch 100 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 102 102
  , H.mkSeq 101 42
  , H.mkSeq 101 103
  , H.mkVar 102 "NOP_102"
  , H.mkSeq 102 103
  , H.mkVar 103 "IF_ELSE_FOOTER"
  , H.mkAssign 104 "left" (Num 0)
  , H.mkSeq 104 105
  , H.mkVar 105 "left_o"
  , H.mkSeq 105 106
  , H.mkBranch 106 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 108 108
  , H.mkSeq 107 42
  , H.mkSeq 107 109
  , H.mkVar 108 "NOP_108"
  , H.mkSeq 108 109
  , H.mkVar 109 "IF_ELSE_FOOTER"
  , H.mkAssign 110 "right" (Num 0)
  , H.mkSeq 110 111
  , H.mkVar 111 "left_o"
  , H.mkSeq 111 112
  , H.mkVar 112 "right_o"
  , H.mkSeq 112 113
  , H.mkVar 113 "dres"
  , H.mkSeq 113 114
  , H.mkAssign 114 "res" (Num 0)
  , H.mkSeq 114 115
  , H.mkBranch 115 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 117 117
  , H.mkSeq 116 1813
  , H.mkSeq 116 118
  , H.mkVar 117 "NOP_117"
  , H.mkSeq 117 118
  , H.mkVar 118 "IF_ELSE_FOOTER"
  , H.mkAssign 119 "undefed" (Num 0)
  , H.mkSeq 119 120
  , H.mkAssign 120 "stack_pointer" (Num 0)
  , H.mkSeq 120 121
  , H.mkVar 121 "word"
  , H.mkSeq 121 122
  , H.mkAssign 122 "opcode" (Num 0)
  , H.mkSeq 122 123
  , H.mkAssign 123 "oparg" (Num 0)
  , H.mkSeq 123 124
  , H.mkBranch 124 (Eq (Num 0) (Num 1)) 125 128
  , H.mkVar 125 "word"
  , H.mkSeq 125 126
  , H.mkAssign 126 "opcode" (Num 0)
  , H.mkSeq 126 127
  , H.mkAssign 127 "oparg" (Num 0)
  , H.mkSeq 127 128
  , H.mkSeq 127 124
  , H.mkVar 128 "LOOP_FOOTER"
  , H.mkSeq 128 129
  , H.mkSeq 128 35
  , H.mkBranch 129 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 131 170
  , H.mkVar 131 "NOP_131"
  , H.mkVar 132 "__CLABEL_TARGET_BINARY_OP_ADD_INT"
  , H.mkSeq 132 133
  , H.mkVar 133 "this_instr"
  , H.mkSeq 133 134
  , H.mkAssign 134 "undefed" (Num 0)
  , H.mkSeq 134 135
  , H.mkAssign 135 "next_instr" (Num 0)
  , H.mkSeq 135 136
  , H.mkVar 136 "value"
  , H.mkSeq 136 137
  , H.mkVar 137 "left"
  , H.mkSeq 137 138
  , H.mkVar 138 "right"
  , H.mkSeq 138 139
  , H.mkVar 139 "res"
  , H.mkSeq 139 140
  , H.mkAssign 140 "value" (Num 0)
  , H.mkSeq 140 141
  , H.mkVar 141 "value_o"
  , H.mkSeq 141 142
  , H.mkBranch 142 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 144 144
  , H.mkSeq 143 42
  , H.mkSeq 143 145
  , H.mkVar 144 "NOP_144"
  , H.mkSeq 144 145
  , H.mkVar 145 "IF_ELSE_FOOTER"
  , H.mkAssign 146 "left" (Num 0)
  , H.mkSeq 146 147
  , H.mkVar 147 "left_o"
  , H.mkSeq 147 148
  , H.mkBranch 148 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 150 150
  , H.mkSeq 149 42
  , H.mkSeq 149 151
  , H.mkVar 150 "NOP_150"
  , H.mkSeq 150 151
  , H.mkVar 151 "IF_ELSE_FOOTER"
  , H.mkAssign 152 "right" (Num 0)
  , H.mkSeq 152 153
  , H.mkVar 153 "left_o"
  , H.mkSeq 153 154
  , H.mkVar 154 "right_o"
  , H.mkSeq 154 155
  , H.mkAssign 155 "res" (Num 0)
  , H.mkSeq 155 156
  , H.mkBranch 156 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 158 158
  , H.mkSeq 157 42
  , H.mkSeq 157 159
  , H.mkVar 158 "NOP_158"
  , H.mkSeq 158 159
  , H.mkVar 159 "IF_ELSE_FOOTER"
  , H.mkAssign 160 "undefed" (Num 0)
  , H.mkSeq 160 161
  , H.mkAssign 161 "stack_pointer" (Num 0)
  , H.mkSeq 161 162
  , H.mkVar 162 "word"
  , H.mkSeq 162 163
  , H.mkAssign 163 "opcode" (Num 0)
  , H.mkSeq 163 164
  , H.mkAssign 164 "oparg" (Num 0)
  , H.mkSeq 164 165
  , H.mkBranch 165 (Eq (Num 0) (Num 1)) 166 169
  , H.mkVar 166 "word"
  , H.mkSeq 166 167
  , H.mkAssign 167 "opcode" (Num 0)
  , H.mkSeq 167 168
  , H.mkAssign 168 "oparg" (Num 0)
  , H.mkSeq 168 169
  , H.mkSeq 168 165
  , H.mkVar 169 "LOOP_FOOTER"
  , H.mkSeq 169 170
  , H.mkSeq 169 35
  , H.mkBranch 170 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 172 214
  , H.mkVar 172 "NOP_172"
  , H.mkVar 173 "__CLABEL_TARGET_BINARY_OP_ADD_UNICODE"
  , H.mkSeq 173 174
  , H.mkVar 174 "this_instr"
  , H.mkSeq 174 175
  , H.mkAssign 175 "undefed" (Num 0)
  , H.mkSeq 175 176
  , H.mkAssign 176 "next_instr" (Num 0)
  , H.mkSeq 176 177
  , H.mkVar 177 "value"
  , H.mkSeq 177 178
  , H.mkVar 178 "nos"
  , H.mkSeq 178 179
  , H.mkVar 179 "left"
  , H.mkSeq 179 180
  , H.mkVar 180 "right"
  , H.mkSeq 180 181
  , H.mkVar 181 "res"
  , H.mkSeq 181 182
  , H.mkAssign 182 "value" (Num 0)
  , H.mkSeq 182 183
  , H.mkVar 183 "value_o"
  , H.mkSeq 183 184
  , H.mkBranch 184 (Eq (Plus (Plus (Num 0) (Plus (Id "PyUnicode_Type") (Num 0))) (Num 0)) (Num 1)) 186 186
  , H.mkSeq 185 42
  , H.mkSeq 185 187
  , H.mkVar 186 "NOP_186"
  , H.mkSeq 186 187
  , H.mkVar 187 "IF_ELSE_FOOTER"
  , H.mkAssign 188 "nos" (Num 0)
  , H.mkSeq 188 189
  , H.mkVar 189 "o"
  , H.mkSeq 189 190
  , H.mkBranch 190 (Eq (Plus (Plus (Num 0) (Plus (Id "PyUnicode_Type") (Num 0))) (Num 0)) (Num 1)) 192 192
  , H.mkSeq 191 42
  , H.mkSeq 191 193
  , H.mkVar 192 "NOP_192"
  , H.mkSeq 192 193
  , H.mkVar 193 "IF_ELSE_FOOTER"
  , H.mkAssign 194 "right" (Num 0)
  , H.mkSeq 194 195
  , H.mkAssign 195 "left" (Num 0)
  , H.mkSeq 195 196
  , H.mkVar 196 "left_o"
  , H.mkSeq 196 197
  , H.mkVar 197 "right_o"
  , H.mkSeq 197 198
  , H.mkVar 198 "res_o"
  , H.mkSeq 198 199
  , H.mkBranch 199 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 201 201
  , H.mkSeq 200 1813
  , H.mkSeq 200 202
  , H.mkVar 201 "NOP_201"
  , H.mkSeq 201 202
  , H.mkVar 202 "IF_ELSE_FOOTER"
  , H.mkAssign 203 "res" (Num 0)
  , H.mkSeq 203 204
  , H.mkAssign 204 "undefed" (Num 0)
  , H.mkSeq 204 205
  , H.mkAssign 205 "stack_pointer" (Num 0)
  , H.mkSeq 205 206
  , H.mkVar 206 "word"
  , H.mkSeq 206 207
  , H.mkAssign 207 "opcode" (Num 0)
  , H.mkSeq 207 208
  , H.mkAssign 208 "oparg" (Num 0)
  , H.mkSeq 208 209
  , H.mkBranch 209 (Eq (Num 0) (Num 1)) 210 213
  , H.mkVar 210 "word"
  , H.mkSeq 210 211
  , H.mkAssign 211 "opcode" (Num 0)
  , H.mkSeq 211 212
  , H.mkAssign 212 "oparg" (Num 0)
  , H.mkSeq 212 213
  , H.mkSeq 212 209
  , H.mkVar 213 "LOOP_FOOTER"
  , H.mkSeq 213 214
  , H.mkSeq 213 35
  , H.mkBranch 214 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 216 260
  , H.mkVar 216 "NOP_216"
  , H.mkVar 217 "__CLABEL_TARGET_BINARY_OP_EXTEND"
  , H.mkSeq 217 218
  , H.mkVar 218 "this_instr"
  , H.mkSeq 218 219
  , H.mkAssign 219 "undefed" (Num 0)
  , H.mkSeq 219 220
  , H.mkAssign 220 "next_instr" (Num 0)
  , H.mkSeq 220 221
  , H.mkVar 221 "left"
  , H.mkSeq 221 222
  , H.mkVar 222 "right"
  , H.mkSeq 222 223
  , H.mkVar 223 "res"
  , H.mkSeq 223 224
  , H.mkAssign 224 "right" (Num 0)
  , H.mkSeq 224 225
  , H.mkAssign 225 "left" (Num 0)
  , H.mkSeq 225 226
  , H.mkVar 226 "descr"
  , H.mkSeq 226 227
  , H.mkVar 227 "left_o"
  , H.mkSeq 227 228
  , H.mkVar 228 "right_o"
  , H.mkSeq 228 229
  , H.mkVar 229 "d"
  , H.mkSeq 229 230
  , H.mkVar 230 "res"
  , H.mkSeq 230 231
  , H.mkAssign 231 "stack_pointer" (Num 0)
  , H.mkSeq 231 232
  , H.mkBranch 232 (Eq (Plus (Id "res") (Num 0)) (Num 1)) 234 234
  , H.mkSeq 233 42
  , H.mkSeq 233 235
  , H.mkVar 234 "NOP_234"
  , H.mkSeq 234 235
  , H.mkVar 235 "IF_ELSE_FOOTER"
  , H.mkVar 236 "descr"
  , H.mkSeq 236 237
  , H.mkVar 237 "left_o"
  , H.mkSeq 237 238
  , H.mkVar 238 "right_o"
  , H.mkSeq 238 239
  , H.mkVar 239 "d"
  , H.mkSeq 239 240
  , H.mkVar 240 "res_o"
  , H.mkSeq 240 241
  , H.mkVar 241 "tmp"
  , H.mkSeq 241 242
  , H.mkAssign 242 "right" (Num 0)
  , H.mkSeq 242 243
  , H.mkAssign 243 "undefed" (Num 0)
  , H.mkSeq 243 244
  , H.mkAssign 244 "tmp" (Num 0)
  , H.mkSeq 244 245
  , H.mkAssign 245 "left" (Num 0)
  , H.mkSeq 245 246
  , H.mkAssign 246 "undefed" (Num 0)
  , H.mkSeq 246 247
  , H.mkAssign 247 "stack_pointer" (Num 0)
  , H.mkSeq 247 248
  , H.mkAssign 248 "stack_pointer" (Num 0)
  , H.mkSeq 248 249
  , H.mkAssign 249 "res" (Num 0)
  , H.mkSeq 249 250
  , H.mkAssign 250 "undefed" (Num 0)
  , H.mkSeq 250 251
  , H.mkAssign 251 "stack_pointer" (Num 0)
  , H.mkSeq 251 252
  , H.mkVar 252 "word"
  , H.mkSeq 252 253
  , H.mkAssign 253 "opcode" (Num 0)
  , H.mkSeq 253 254
  , H.mkAssign 254 "oparg" (Num 0)
  , H.mkSeq 254 255
  , H.mkBranch 255 (Eq (Num 0) (Num 1)) 256 259
  , H.mkVar 256 "word"
  , H.mkSeq 256 257
  , H.mkAssign 257 "opcode" (Num 0)
  , H.mkSeq 257 258
  , H.mkAssign 258 "oparg" (Num 0)
  , H.mkSeq 258 259
  , H.mkSeq 258 255
  , H.mkVar 259 "LOOP_FOOTER"
  , H.mkSeq 259 260
  , H.mkSeq 259 35
  , H.mkBranch 260 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 262 354
  , H.mkVar 262 "NOP_262"
  , H.mkVar 263 "__CLABEL_TARGET_BINARY_OP_INPLACE_ADD_UNICODE"
  , H.mkSeq 263 264
  , H.mkVar 264 "this_instr"
  , H.mkSeq 264 265
  , H.mkAssign 265 "undefed" (Num 0)
  , H.mkSeq 265 266
  , H.mkAssign 266 "next_instr" (Num 0)
  , H.mkSeq 266 267
  , H.mkVar 267 "value"
  , H.mkSeq 267 268
  , H.mkVar 268 "nos"
  , H.mkSeq 268 269
  , H.mkVar 269 "left"
  , H.mkSeq 269 270
  , H.mkVar 270 "right"
  , H.mkSeq 270 271
  , H.mkAssign 271 "value" (Num 0)
  , H.mkSeq 271 272
  , H.mkVar 272 "value_o"
  , H.mkSeq 272 273
  , H.mkBranch 273 (Eq (Plus (Plus (Num 0) (Plus (Id "PyUnicode_Type") (Num 0))) (Num 0)) (Num 1)) 275 275
  , H.mkSeq 274 42
  , H.mkSeq 274 276
  , H.mkVar 275 "NOP_275"
  , H.mkSeq 275 276
  , H.mkVar 276 "IF_ELSE_FOOTER"
  , H.mkAssign 277 "nos" (Num 0)
  , H.mkSeq 277 278
  , H.mkVar 278 "o"
  , H.mkSeq 278 279
  , H.mkBranch 279 (Eq (Plus (Plus (Num 0) (Plus (Id "PyUnicode_Type") (Num 0))) (Num 0)) (Num 1)) 281 281
  , H.mkSeq 280 42
  , H.mkSeq 280 282
  , H.mkVar 281 "NOP_281"
  , H.mkSeq 281 282
  , H.mkVar 282 "IF_ELSE_FOOTER"
  , H.mkAssign 283 "right" (Num 0)
  , H.mkSeq 283 284
  , H.mkAssign 284 "left" (Num 0)
  , H.mkSeq 284 285
  , H.mkVar 285 "left_o"
  , H.mkSeq 285 286
  , H.mkVar 286 "next_oparg"
  , H.mkSeq 286 287
  , H.mkAssign 287 "next_oparg" (Num 0)
  , H.mkSeq 287 288
  , H.mkVar 288 "target_local"
  , H.mkSeq 288 289
  , H.mkBranch 289 (Eq (Plus (Num 0) (Id "left_o")) (Num 1)) 291 291
  , H.mkSeq 290 42
  , H.mkSeq 290 292
  , H.mkVar 291 "NOP_291"
  , H.mkSeq 291 292
  , H.mkVar 292 "IF_ELSE_FOOTER"
  , H.mkVar 293 "temp"
  , H.mkSeq 293 294
  , H.mkVar 294 "right_o"
  , H.mkSeq 294 295
  , H.mkAssign 295 "stack_pointer" (Num 0)
  , H.mkSeq 295 296
  , H.mkAssign 296 "stack_pointer" (Num 0)
  , H.mkSeq 296 297
  , H.mkAssign 297 "undefed" (Num 0)
  , H.mkSeq 297 298
  , H.mkVar 298 "op"
  , H.mkSeq 298 299
  , H.mkBranch 299 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 301 316
  , H.mkVar 301 "tracer"
  , H.mkSeq 301 302
  , H.mkBranch 302 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 304 305
  , H.mkVar 304 "data"
  , H.mkSeq 304 305
  , H.mkSeq 304 306
  , H.mkVar 305 "NOP_305"
  , H.mkSeq 305 306
  , H.mkVar 306 "IF_ELSE_FOOTER"
  , H.mkBranch 307 (Eq (Num 0) (Num 1)) 308 314
  , H.mkVar 308 "tracer"
  , H.mkSeq 308 309
  , H.mkBranch 309 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 311 312
  , H.mkVar 311 "data"
  , H.mkSeq 311 312
  , H.mkSeq 311 313
  , H.mkVar 312 "NOP_312"
  , H.mkSeq 312 313
  , H.mkVar 313 "IF_ELSE_FOOTER"
  , H.mkSeq 313 307
  , H.mkVar 314 "LOOP_FOOTER"
  , H.mkSeq 314 315
  , H.mkVar 315 "dealloc"
  , H.mkSeq 315 316
  , H.mkSeq 315 317
  , H.mkVar 316 "NOP_316"
  , H.mkSeq 316 317
  , H.mkVar 317 "IF_ELSE_FOOTER"
  , H.mkBranch 318 (Eq (Num 0) (Num 1)) 319 339
  , H.mkVar 319 "op"
  , H.mkSeq 319 320
  , H.mkBranch 320 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 322 337
  , H.mkVar 322 "tracer"
  , H.mkSeq 322 323
  , H.mkBranch 323 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 325 326
  , H.mkVar 325 "data"
  , H.mkSeq 325 326
  , H.mkSeq 325 327
  , H.mkVar 326 "NOP_326"
  , H.mkSeq 326 327
  , H.mkVar 327 "IF_ELSE_FOOTER"
  , H.mkBranch 328 (Eq (Num 0) (Num 1)) 329 335
  , H.mkVar 329 "tracer"
  , H.mkSeq 329 330
  , H.mkBranch 330 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 332 333
  , H.mkVar 332 "data"
  , H.mkSeq 332 333
  , H.mkSeq 332 334
  , H.mkVar 333 "NOP_333"
  , H.mkSeq 333 334
  , H.mkVar 334 "IF_ELSE_FOOTER"
  , H.mkSeq 334 328
  , H.mkVar 335 "LOOP_FOOTER"
  , H.mkSeq 335 336
  , H.mkVar 336 "dealloc"
  , H.mkSeq 336 337
  , H.mkSeq 336 338
  , H.mkVar 337 "NOP_337"
  , H.mkSeq 337 338
  , H.mkVar 338 "IF_ELSE_FOOTER"
  , H.mkSeq 338 318
  , H.mkVar 339 "LOOP_FOOTER"
  , H.mkSeq 339 340
  , H.mkAssign 340 "stack_pointer" (Num 0)
  , H.mkSeq 340 341
  , H.mkBranch 341 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 343 343
  , H.mkSeq 342 1817
  , H.mkSeq 342 344
  , H.mkVar 343 "NOP_343"
  , H.mkSeq 343 344
  , H.mkVar 344 "IF_ELSE_FOOTER"
  , H.mkAssign 345 "next_instr" (Num 0)
  , H.mkSeq 345 346
  , H.mkVar 346 "word"
  , H.mkSeq 346 347
  , H.mkAssign 347 "opcode" (Num 0)
  , H.mkSeq 347 348
  , H.mkAssign 348 "oparg" (Num 0)
  , H.mkSeq 348 349
  , H.mkBranch 349 (Eq (Num 0) (Num 1)) 350 353
  , H.mkVar 350 "word"
  , H.mkSeq 350 351
  , H.mkAssign 351 "opcode" (Num 0)
  , H.mkSeq 351 352
  , H.mkAssign 352 "oparg" (Num 0)
  , H.mkSeq 352 353
  , H.mkSeq 352 349
  , H.mkVar 353 "LOOP_FOOTER"
  , H.mkSeq 353 354
  , H.mkSeq 353 35
  , H.mkBranch 354 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 356 396
  , H.mkVar 356 "NOP_356"
  , H.mkVar 357 "__CLABEL_TARGET_BINARY_OP_MULTIPLY_FLOAT"
  , H.mkSeq 357 358
  , H.mkVar 358 "this_instr"
  , H.mkSeq 358 359
  , H.mkAssign 359 "undefed" (Num 0)
  , H.mkSeq 359 360
  , H.mkAssign 360 "next_instr" (Num 0)
  , H.mkSeq 360 361
  , H.mkVar 361 "value"
  , H.mkSeq 361 362
  , H.mkVar 362 "left"
  , H.mkSeq 362 363
  , H.mkVar 363 "right"
  , H.mkSeq 363 364
  , H.mkVar 364 "res"
  , H.mkSeq 364 365
  , H.mkAssign 365 "value" (Num 0)
  , H.mkSeq 365 366
  , H.mkVar 366 "value_o"
  , H.mkSeq 366 367
  , H.mkBranch 367 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 369 369
  , H.mkSeq 368 42
  , H.mkSeq 368 370
  , H.mkVar 369 "NOP_369"
  , H.mkSeq 369 370
  , H.mkVar 370 "IF_ELSE_FOOTER"
  , H.mkAssign 371 "left" (Num 0)
  , H.mkSeq 371 372
  , H.mkVar 372 "left_o"
  , H.mkSeq 372 373
  , H.mkBranch 373 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 375 375
  , H.mkSeq 374 42
  , H.mkSeq 374 376
  , H.mkVar 375 "NOP_375"
  , H.mkSeq 375 376
  , H.mkVar 376 "IF_ELSE_FOOTER"
  , H.mkAssign 377 "right" (Num 0)
  , H.mkSeq 377 378
  , H.mkVar 378 "left_o"
  , H.mkSeq 378 379
  , H.mkVar 379 "right_o"
  , H.mkSeq 379 380
  , H.mkVar 380 "dres"
  , H.mkSeq 380 381
  , H.mkAssign 381 "res" (Num 0)
  , H.mkSeq 381 382
  , H.mkBranch 382 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 384 384
  , H.mkSeq 383 1813
  , H.mkSeq 383 385
  , H.mkVar 384 "NOP_384"
  , H.mkSeq 384 385
  , H.mkVar 385 "IF_ELSE_FOOTER"
  , H.mkAssign 386 "undefed" (Num 0)
  , H.mkSeq 386 387
  , H.mkAssign 387 "stack_pointer" (Num 0)
  , H.mkSeq 387 388
  , H.mkVar 388 "word"
  , H.mkSeq 388 389
  , H.mkAssign 389 "opcode" (Num 0)
  , H.mkSeq 389 390
  , H.mkAssign 390 "oparg" (Num 0)
  , H.mkSeq 390 391
  , H.mkBranch 391 (Eq (Num 0) (Num 1)) 392 395
  , H.mkVar 392 "word"
  , H.mkSeq 392 393
  , H.mkAssign 393 "opcode" (Num 0)
  , H.mkSeq 393 394
  , H.mkAssign 394 "oparg" (Num 0)
  , H.mkSeq 394 395
  , H.mkSeq 394 391
  , H.mkVar 395 "LOOP_FOOTER"
  , H.mkSeq 395 396
  , H.mkSeq 395 35
  , H.mkBranch 396 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 398 437
  , H.mkVar 398 "NOP_398"
  , H.mkVar 399 "__CLABEL_TARGET_BINARY_OP_MULTIPLY_INT"
  , H.mkSeq 399 400
  , H.mkVar 400 "this_instr"
  , H.mkSeq 400 401
  , H.mkAssign 401 "undefed" (Num 0)
  , H.mkSeq 401 402
  , H.mkAssign 402 "next_instr" (Num 0)
  , H.mkSeq 402 403
  , H.mkVar 403 "value"
  , H.mkSeq 403 404
  , H.mkVar 404 "left"
  , H.mkSeq 404 405
  , H.mkVar 405 "right"
  , H.mkSeq 405 406
  , H.mkVar 406 "res"
  , H.mkSeq 406 407
  , H.mkAssign 407 "value" (Num 0)
  , H.mkSeq 407 408
  , H.mkVar 408 "value_o"
  , H.mkSeq 408 409
  , H.mkBranch 409 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 411 411
  , H.mkSeq 410 42
  , H.mkSeq 410 412
  , H.mkVar 411 "NOP_411"
  , H.mkSeq 411 412
  , H.mkVar 412 "IF_ELSE_FOOTER"
  , H.mkAssign 413 "left" (Num 0)
  , H.mkSeq 413 414
  , H.mkVar 414 "left_o"
  , H.mkSeq 414 415
  , H.mkBranch 415 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 417 417
  , H.mkSeq 416 42
  , H.mkSeq 416 418
  , H.mkVar 417 "NOP_417"
  , H.mkSeq 417 418
  , H.mkVar 418 "IF_ELSE_FOOTER"
  , H.mkAssign 419 "right" (Num 0)
  , H.mkSeq 419 420
  , H.mkVar 420 "left_o"
  , H.mkSeq 420 421
  , H.mkVar 421 "right_o"
  , H.mkSeq 421 422
  , H.mkAssign 422 "res" (Num 0)
  , H.mkSeq 422 423
  , H.mkBranch 423 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 425 425
  , H.mkSeq 424 42
  , H.mkSeq 424 426
  , H.mkVar 425 "NOP_425"
  , H.mkSeq 425 426
  , H.mkVar 426 "IF_ELSE_FOOTER"
  , H.mkAssign 427 "undefed" (Num 0)
  , H.mkSeq 427 428
  , H.mkAssign 428 "stack_pointer" (Num 0)
  , H.mkSeq 428 429
  , H.mkVar 429 "word"
  , H.mkSeq 429 430
  , H.mkAssign 430 "opcode" (Num 0)
  , H.mkSeq 430 431
  , H.mkAssign 431 "oparg" (Num 0)
  , H.mkSeq 431 432
  , H.mkBranch 432 (Eq (Num 0) (Num 1)) 433 436
  , H.mkVar 433 "word"
  , H.mkSeq 433 434
  , H.mkAssign 434 "opcode" (Num 0)
  , H.mkSeq 434 435
  , H.mkAssign 435 "oparg" (Num 0)
  , H.mkSeq 435 436
  , H.mkSeq 435 432
  , H.mkVar 436 "LOOP_FOOTER"
  , H.mkSeq 436 437
  , H.mkSeq 436 35
  , H.mkBranch 437 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 439 489
  , H.mkVar 439 "NOP_439"
  , H.mkVar 440 "__CLABEL_TARGET_BINARY_OP_SUBSCR_DICT"
  , H.mkSeq 440 441
  , H.mkVar 441 "this_instr"
  , H.mkSeq 441 442
  , H.mkAssign 442 "undefed" (Num 0)
  , H.mkSeq 442 443
  , H.mkAssign 443 "next_instr" (Num 0)
  , H.mkSeq 443 444
  , H.mkVar 444 "nos"
  , H.mkSeq 444 445
  , H.mkVar 445 "dict_st"
  , H.mkSeq 445 446
  , H.mkVar 446 "sub_st"
  , H.mkSeq 446 447
  , H.mkVar 447 "res"
  , H.mkSeq 447 448
  , H.mkAssign 448 "nos" (Num 0)
  , H.mkSeq 448 449
  , H.mkVar 449 "o"
  , H.mkSeq 449 450
  , H.mkBranch 450 (Eq (Plus (Plus (Num 0) (Plus (Id "PyDict_Type") (Num 0))) (Num 0)) (Num 1)) 452 452
  , H.mkSeq 451 42
  , H.mkSeq 451 453
  , H.mkVar 452 "NOP_452"
  , H.mkSeq 452 453
  , H.mkVar 453 "IF_ELSE_FOOTER"
  , H.mkAssign 454 "sub_st" (Num 0)
  , H.mkSeq 454 455
  , H.mkAssign 455 "dict_st" (Num 0)
  , H.mkSeq 455 456
  , H.mkVar 456 "sub"
  , H.mkSeq 456 457
  , H.mkVar 457 "dict"
  , H.mkSeq 457 458
  , H.mkVar 458 "res_o"
  , H.mkSeq 458 459
  , H.mkVar 459 "rc"
  , H.mkSeq 459 460
  , H.mkAssign 460 "stack_pointer" (Num 0)
  , H.mkSeq 460 461
  , H.mkBranch 461 (Eq (Plus (Id "rc") (Num 0)) (Num 1)) 463 464
  , H.mkAssign 463 "stack_pointer" (Num 0)
  , H.mkSeq 463 465
  , H.mkVar 464 "NOP_464"
  , H.mkSeq 464 465
  , H.mkVar 465 "IF_ELSE_FOOTER"
  , H.mkVar 466 "tmp"
  , H.mkSeq 466 467
  , H.mkAssign 467 "sub_st" (Num 0)
  , H.mkSeq 467 468
  , H.mkAssign 468 "undefed" (Num 0)
  , H.mkSeq 468 469
  , H.mkAssign 469 "tmp" (Num 0)
  , H.mkSeq 469 470
  , H.mkAssign 470 "dict_st" (Num 0)
  , H.mkSeq 470 471
  , H.mkAssign 471 "undefed" (Num 0)
  , H.mkSeq 471 472
  , H.mkAssign 472 "stack_pointer" (Num 0)
  , H.mkSeq 472 473
  , H.mkAssign 473 "stack_pointer" (Num 0)
  , H.mkSeq 473 474
  , H.mkBranch 474 (Eq (Plus (Id "rc") (Num 0)) (Num 1)) 476 476
  , H.mkSeq 475 1817
  , H.mkSeq 475 477
  , H.mkVar 476 "NOP_476"
  , H.mkSeq 476 477
  , H.mkVar 477 "IF_ELSE_FOOTER"
  , H.mkAssign 478 "res" (Num 0)
  , H.mkSeq 478 479
  , H.mkAssign 479 "undefed" (Num 0)
  , H.mkSeq 479 480
  , H.mkAssign 480 "stack_pointer" (Num 0)
  , H.mkSeq 480 481
  , H.mkVar 481 "word"
  , H.mkSeq 481 482
  , H.mkAssign 482 "opcode" (Num 0)
  , H.mkSeq 482 483
  , H.mkAssign 483 "oparg" (Num 0)
  , H.mkSeq 483 484
  , H.mkBranch 484 (Eq (Num 0) (Num 1)) 485 488
  , H.mkVar 485 "word"
  , H.mkSeq 485 486
  , H.mkAssign 486 "opcode" (Num 0)
  , H.mkSeq 486 487
  , H.mkAssign 487 "oparg" (Num 0)
  , H.mkSeq 487 488
  , H.mkSeq 487 484
  , H.mkVar 488 "LOOP_FOOTER"
  , H.mkSeq 488 489
  , H.mkSeq 488 35
  , H.mkBranch 489 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 491 549
  , H.mkVar 491 "NOP_491"
  , H.mkVar 492 "__CLABEL_TARGET_BINARY_OP_SUBSCR_GETITEM"
  , H.mkSeq 492 493
  , H.mkVar 493 "this_instr"
  , H.mkSeq 493 494
  , H.mkAssign 494 "undefed" (Num 0)
  , H.mkSeq 494 495
  , H.mkAssign 495 "next_instr" (Num 0)
  , H.mkSeq 495 496
  , H.mkVar 496 "container"
  , H.mkSeq 496 497
  , H.mkVar 497 "getitem"
  , H.mkSeq 497 498
  , H.mkVar 498 "sub"
  , H.mkSeq 498 499
  , H.mkVar 499 "new_frame"
  , H.mkSeq 499 500
  , H.mkBranch 500 (Eq (Num 0) (Num 1)) 502 502
  , H.mkSeq 501 42
  , H.mkSeq 501 503
  , H.mkVar 502 "NOP_502"
  , H.mkSeq 502 503
  , H.mkVar 503 "IF_ELSE_FOOTER"
  , H.mkAssign 504 "container" (Num 0)
  , H.mkSeq 504 505
  , H.mkVar 505 "tp"
  , H.mkSeq 505 506
  , H.mkBranch 506 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 508 508
  , H.mkSeq 507 42
  , H.mkSeq 507 509
  , H.mkVar 508 "NOP_508"
  , H.mkSeq 508 509
  , H.mkVar 509 "IF_ELSE_FOOTER"
  , H.mkVar 510 "ht"
  , H.mkSeq 510 511
  , H.mkVar 511 "getitem_o"
  , H.mkSeq 511 512
  , H.mkBranch 512 (Eq (Plus (Id "getitem_o") (Num 0)) (Num 1)) 514 514
  , H.mkSeq 513 42
  , H.mkSeq 513 515
  , H.mkVar 514 "NOP_514"
  , H.mkSeq 514 515
  , H.mkVar 515 "IF_ELSE_FOOTER"
  , H.mkVar 516 "cached_version"
  , H.mkSeq 516 517
  , H.mkBranch 517 (Eq (Plus (Num 0) (Id "cached_version")) (Num 1)) 519 519
  , H.mkSeq 518 42
  , H.mkSeq 518 520
  , H.mkVar 519 "NOP_519"
  , H.mkSeq 519 520
  , H.mkVar 520 "IF_ELSE_FOOTER"
  , H.mkVar 521 "code"
  , H.mkSeq 521 522
  , H.mkBranch 522 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 524 524
  , H.mkSeq 523 42
  , H.mkSeq 523 525
  , H.mkVar 524 "NOP_524"
  , H.mkSeq 524 525
  , H.mkVar 525 "IF_ELSE_FOOTER"
  , H.mkAssign 526 "getitem" (Num 0)
  , H.mkSeq 526 527
  , H.mkAssign 527 "sub" (Num 0)
  , H.mkSeq 527 528
  , H.mkVar 528 "pushed_frame"
  , H.mkSeq 528 529
  , H.mkAssign 529 "undefed" (Num 0)
  , H.mkSeq 529 530
  , H.mkAssign 530 "undefed" (Num 0)
  , H.mkSeq 530 531
  , H.mkAssign 531 "undefed" (Num 0)
  , H.mkSeq 531 532
  , H.mkAssign 532 "new_frame" (Num 0)
  , H.mkSeq 532 533
  , H.mkVar 533 "temp"
  , H.mkSeq 533 534
  , H.mkAssign 534 "stack_pointer" (Num 0)
  , H.mkSeq 534 535
  , H.mkAssign 535 "frame" (Num 0)
  , H.mkSeq 535 536
  , H.mkAssign 536 "stack_pointer" (Num 0)
  , H.mkSeq 536 537
  , H.mkAssign 537 "next_instr" (Num 0)
  , H.mkSeq 537 538
  , H.mkBranch 538 (Eq (Num 0) (Num 1)) 539 540
  , H.mkAssign 539 "next_instr" (Num 0)
  , H.mkSeq 539 538
  , H.mkVar 540 "LOOP_FOOTER"
  , H.mkSeq 540 541
  , H.mkVar 541 "word"
  , H.mkSeq 541 542
  , H.mkAssign 542 "opcode" (Num 0)
  , H.mkSeq 542 543
  , H.mkAssign 543 "oparg" (Num 0)
  , H.mkSeq 543 544
  , H.mkBranch 544 (Eq (Num 0) (Num 1)) 545 548
  , H.mkVar 545 "word"
  , H.mkSeq 545 546
  , H.mkAssign 546 "opcode" (Num 0)
  , H.mkSeq 546 547
  , H.mkAssign 547 "oparg" (Num 0)
  , H.mkSeq 547 548
  , H.mkSeq 547 544
  , H.mkVar 548 "LOOP_FOOTER"
  , H.mkSeq 548 549
  , H.mkSeq 548 35
  , H.mkBranch 549 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 551 604
  , H.mkVar 551 "NOP_551"
  , H.mkVar 552 "__CLABEL_TARGET_BINARY_OP_SUBSCR_LIST_INT"
  , H.mkSeq 552 553
  , H.mkVar 553 "this_instr"
  , H.mkSeq 553 554
  , H.mkAssign 554 "undefed" (Num 0)
  , H.mkSeq 554 555
  , H.mkAssign 555 "next_instr" (Num 0)
  , H.mkSeq 555 556
  , H.mkVar 556 "value"
  , H.mkSeq 556 557
  , H.mkVar 557 "nos"
  , H.mkSeq 557 558
  , H.mkVar 558 "list_st"
  , H.mkSeq 558 559
  , H.mkVar 559 "sub_st"
  , H.mkSeq 559 560
  , H.mkVar 560 "res"
  , H.mkSeq 560 561
  , H.mkAssign 561 "value" (Num 0)
  , H.mkSeq 561 562
  , H.mkVar 562 "value_o"
  , H.mkSeq 562 563
  , H.mkBranch 563 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 565 565
  , H.mkSeq 564 42
  , H.mkSeq 564 566
  , H.mkVar 565 "NOP_565"
  , H.mkSeq 565 566
  , H.mkVar 566 "IF_ELSE_FOOTER"
  , H.mkAssign 567 "nos" (Num 0)
  , H.mkSeq 567 568
  , H.mkVar 568 "o"
  , H.mkSeq 568 569
  , H.mkBranch 569 (Eq (Plus (Plus (Num 0) (Plus (Id "PyList_Type") (Num 0))) (Num 0)) (Num 1)) 571 571
  , H.mkSeq 570 42
  , H.mkSeq 570 572
  , H.mkVar 571 "NOP_571"
  , H.mkSeq 571 572
  , H.mkVar 572 "IF_ELSE_FOOTER"
  , H.mkAssign 573 "sub_st" (Num 0)
  , H.mkSeq 573 574
  , H.mkAssign 574 "list_st" (Num 0)
  , H.mkSeq 574 575
  , H.mkVar 575 "sub"
  , H.mkSeq 575 576
  , H.mkVar 576 "list"
  , H.mkSeq 576 577
  , H.mkBranch 577 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 579 579
  , H.mkSeq 578 42
  , H.mkSeq 578 580
  , H.mkVar 579 "NOP_579"
  , H.mkSeq 579 580
  , H.mkVar 580 "IF_ELSE_FOOTER"
  , H.mkVar 581 "index"
  , H.mkSeq 581 582
  , H.mkBranch 582 (Eq (Plus (Id "index") (Num 0)) (Num 1)) 584 584
  , H.mkSeq 583 42
  , H.mkSeq 583 585
  , H.mkVar 584 "NOP_584"
  , H.mkSeq 584 585
  , H.mkVar 585 "IF_ELSE_FOOTER"
  , H.mkVar 586 "res_o"
  , H.mkSeq 586 587
  , H.mkAssign 587 "res" (Num 0)
  , H.mkSeq 587 588
  , H.mkVar 588 "tmp"
  , H.mkSeq 588 589
  , H.mkAssign 589 "list_st" (Num 0)
  , H.mkSeq 589 590
  , H.mkAssign 590 "undefed" (Num 0)
  , H.mkSeq 590 591
  , H.mkAssign 591 "tmp" (Num 0)
  , H.mkSeq 591 592
  , H.mkAssign 592 "sub_st" (Num 0)
  , H.mkSeq 592 593
  , H.mkAssign 593 "undefed" (Num 0)
  , H.mkSeq 593 594
  , H.mkAssign 594 "stack_pointer" (Num 0)
  , H.mkSeq 594 595
  , H.mkAssign 595 "stack_pointer" (Num 0)
  , H.mkSeq 595 596
  , H.mkVar 596 "word"
  , H.mkSeq 596 597
  , H.mkAssign 597 "opcode" (Num 0)
  , H.mkSeq 597 598
  , H.mkAssign 598 "oparg" (Num 0)
  , H.mkSeq 598 599
  , H.mkBranch 599 (Eq (Num 0) (Num 1)) 600 603
  , H.mkVar 600 "word"
  , H.mkSeq 600 601
  , H.mkAssign 601 "opcode" (Num 0)
  , H.mkSeq 601 602
  , H.mkAssign 602 "oparg" (Num 0)
  , H.mkSeq 602 603
  , H.mkSeq 602 599
  , H.mkVar 603 "LOOP_FOOTER"
  , H.mkSeq 603 604
  , H.mkSeq 603 35
  , H.mkBranch 604 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 606 657
  , H.mkVar 606 "NOP_606"
  , H.mkVar 607 "__CLABEL_TARGET_BINARY_OP_SUBSCR_LIST_SLICE"
  , H.mkSeq 607 608
  , H.mkVar 608 "this_instr"
  , H.mkSeq 608 609
  , H.mkAssign 609 "undefed" (Num 0)
  , H.mkSeq 609 610
  , H.mkAssign 610 "next_instr" (Num 0)
  , H.mkSeq 610 611
  , H.mkVar 611 "tos"
  , H.mkSeq 611 612
  , H.mkVar 612 "nos"
  , H.mkSeq 612 613
  , H.mkVar 613 "list_st"
  , H.mkSeq 613 614
  , H.mkVar 614 "sub_st"
  , H.mkSeq 614 615
  , H.mkVar 615 "res"
  , H.mkSeq 615 616
  , H.mkAssign 616 "tos" (Num 0)
  , H.mkSeq 616 617
  , H.mkVar 617 "o"
  , H.mkSeq 617 618
  , H.mkBranch 618 (Eq (Plus (Plus (Num 0) (Plus (Id "PySlice_Type") (Num 0))) (Num 0)) (Num 1)) 620 620
  , H.mkSeq 619 42
  , H.mkSeq 619 621
  , H.mkVar 620 "NOP_620"
  , H.mkSeq 620 621
  , H.mkVar 621 "IF_ELSE_FOOTER"
  , H.mkAssign 622 "nos" (Num 0)
  , H.mkSeq 622 623
  , H.mkVar 623 "o"
  , H.mkSeq 623 624
  , H.mkBranch 624 (Eq (Plus (Plus (Num 0) (Plus (Id "PyList_Type") (Num 0))) (Num 0)) (Num 1)) 626 626
  , H.mkSeq 625 42
  , H.mkSeq 625 627
  , H.mkVar 626 "NOP_626"
  , H.mkSeq 626 627
  , H.mkVar 627 "IF_ELSE_FOOTER"
  , H.mkAssign 628 "sub_st" (Num 0)
  , H.mkSeq 628 629
  , H.mkAssign 629 "list_st" (Num 0)
  , H.mkSeq 629 630
  , H.mkVar 630 "sub"
  , H.mkSeq 630 631
  , H.mkVar 631 "list"
  , H.mkSeq 631 632
  , H.mkVar 632 "res_o"
  , H.mkSeq 632 633
  , H.mkAssign 633 "stack_pointer" (Num 0)
  , H.mkSeq 633 634
  , H.mkVar 634 "tmp"
  , H.mkSeq 634 635
  , H.mkAssign 635 "sub_st" (Num 0)
  , H.mkSeq 635 636
  , H.mkAssign 636 "undefed" (Num 0)
  , H.mkSeq 636 637
  , H.mkAssign 637 "tmp" (Num 0)
  , H.mkSeq 637 638
  , H.mkAssign 638 "list_st" (Num 0)
  , H.mkSeq 638 639
  , H.mkAssign 639 "undefed" (Num 0)
  , H.mkSeq 639 640
  , H.mkAssign 640 "stack_pointer" (Num 0)
  , H.mkSeq 640 641
  , H.mkAssign 641 "stack_pointer" (Num 0)
  , H.mkSeq 641 642
  , H.mkBranch 642 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 644 644
  , H.mkSeq 643 1817
  , H.mkSeq 643 645
  , H.mkVar 644 "NOP_644"
  , H.mkSeq 644 645
  , H.mkVar 645 "IF_ELSE_FOOTER"
  , H.mkAssign 646 "res" (Num 0)
  , H.mkSeq 646 647
  , H.mkAssign 647 "undefed" (Num 0)
  , H.mkSeq 647 648
  , H.mkAssign 648 "stack_pointer" (Num 0)
  , H.mkSeq 648 649
  , H.mkVar 649 "word"
  , H.mkSeq 649 650
  , H.mkAssign 650 "opcode" (Num 0)
  , H.mkSeq 650 651
  , H.mkAssign 651 "oparg" (Num 0)
  , H.mkSeq 651 652
  , H.mkBranch 652 (Eq (Num 0) (Num 1)) 653 656
  , H.mkVar 653 "word"
  , H.mkSeq 653 654
  , H.mkAssign 654 "opcode" (Num 0)
  , H.mkSeq 654 655
  , H.mkAssign 655 "oparg" (Num 0)
  , H.mkSeq 655 656
  , H.mkSeq 655 652
  , H.mkVar 656 "LOOP_FOOTER"
  , H.mkSeq 656 657
  , H.mkSeq 656 35
  , H.mkBranch 657 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 659 713
  , H.mkVar 659 "NOP_659"
  , H.mkVar 660 "__CLABEL_TARGET_BINARY_OP_SUBSCR_STR_INT"
  , H.mkSeq 660 661
  , H.mkVar 661 "this_instr"
  , H.mkSeq 661 662
  , H.mkAssign 662 "undefed" (Num 0)
  , H.mkSeq 662 663
  , H.mkAssign 663 "next_instr" (Num 0)
  , H.mkSeq 663 664
  , H.mkVar 664 "value"
  , H.mkSeq 664 665
  , H.mkVar 665 "nos"
  , H.mkSeq 665 666
  , H.mkVar 666 "str_st"
  , H.mkSeq 666 667
  , H.mkVar 667 "sub_st"
  , H.mkSeq 667 668
  , H.mkVar 668 "res"
  , H.mkSeq 668 669
  , H.mkAssign 669 "value" (Num 0)
  , H.mkSeq 669 670
  , H.mkVar 670 "value_o"
  , H.mkSeq 670 671
  , H.mkBranch 671 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 673 673
  , H.mkSeq 672 42
  , H.mkSeq 672 674
  , H.mkVar 673 "NOP_673"
  , H.mkSeq 673 674
  , H.mkVar 674 "IF_ELSE_FOOTER"
  , H.mkAssign 675 "nos" (Num 0)
  , H.mkSeq 675 676
  , H.mkVar 676 "o"
  , H.mkSeq 676 677
  , H.mkBranch 677 (Eq (Plus (Plus (Num 0) (Plus (Id "PyUnicode_Type") (Num 0))) (Num 0)) (Num 1)) 679 679
  , H.mkSeq 678 42
  , H.mkSeq 678 680
  , H.mkVar 679 "NOP_679"
  , H.mkSeq 679 680
  , H.mkVar 680 "IF_ELSE_FOOTER"
  , H.mkAssign 681 "sub_st" (Num 0)
  , H.mkSeq 681 682
  , H.mkAssign 682 "str_st" (Num 0)
  , H.mkSeq 682 683
  , H.mkVar 683 "sub"
  , H.mkSeq 683 684
  , H.mkVar 684 "str"
  , H.mkSeq 684 685
  , H.mkBranch 685 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 687 687
  , H.mkSeq 686 42
  , H.mkSeq 686 688
  , H.mkVar 687 "NOP_687"
  , H.mkSeq 687 688
  , H.mkVar 688 "IF_ELSE_FOOTER"
  , H.mkVar 689 "index"
  , H.mkSeq 689 690
  , H.mkBranch 690 (Eq (Plus (Num 0) (Id "index")) (Num 1)) 692 692
  , H.mkSeq 691 42
  , H.mkSeq 691 693
  , H.mkVar 692 "NOP_692"
  , H.mkSeq 692 693
  , H.mkVar 693 "IF_ELSE_FOOTER"
  , H.mkVar 694 "c"
  , H.mkSeq 694 695
  , H.mkBranch 695 (Eq (Plus (Plus (Plus (Num 0) (Num 0)) (Plus (Num 0) (Num 0))) (Id "c")) (Num 1)) 697 697
  , H.mkSeq 696 42
  , H.mkSeq 696 698
  , H.mkVar 697 "NOP_697"
  , H.mkSeq 697 698
  , H.mkVar 698 "IF_ELSE_FOOTER"
  , H.mkVar 699 "res_o"
  , H.mkSeq 699 700
  , H.mkAssign 700 "stack_pointer" (Num 0)
  , H.mkSeq 700 701
  , H.mkAssign 701 "stack_pointer" (Num 0)
  , H.mkSeq 701 702
  , H.mkAssign 702 "res" (Num 0)
  , H.mkSeq 702 703
  , H.mkAssign 703 "undefed" (Num 0)
  , H.mkSeq 703 704
  , H.mkAssign 704 "stack_pointer" (Num 0)
  , H.mkSeq 704 705
  , H.mkVar 705 "word"
  , H.mkSeq 705 706
  , H.mkAssign 706 "opcode" (Num 0)
  , H.mkSeq 706 707
  , H.mkAssign 707 "oparg" (Num 0)
  , H.mkSeq 707 708
  , H.mkBranch 708 (Eq (Num 0) (Num 1)) 709 712
  , H.mkVar 709 "word"
  , H.mkSeq 709 710
  , H.mkAssign 710 "opcode" (Num 0)
  , H.mkSeq 710 711
  , H.mkAssign 711 "oparg" (Num 0)
  , H.mkSeq 711 712
  , H.mkSeq 711 708
  , H.mkVar 712 "LOOP_FOOTER"
  , H.mkSeq 712 713
  , H.mkSeq 712 35
  , H.mkBranch 713 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 715 765
  , H.mkVar 715 "NOP_715"
  , H.mkVar 716 "__CLABEL_TARGET_BINARY_OP_SUBSCR_TUPLE_INT"
  , H.mkSeq 716 717
  , H.mkVar 717 "this_instr"
  , H.mkSeq 717 718
  , H.mkAssign 718 "undefed" (Num 0)
  , H.mkSeq 718 719
  , H.mkAssign 719 "next_instr" (Num 0)
  , H.mkSeq 719 720
  , H.mkVar 720 "value"
  , H.mkSeq 720 721
  , H.mkVar 721 "nos"
  , H.mkSeq 721 722
  , H.mkVar 722 "tuple_st"
  , H.mkSeq 722 723
  , H.mkVar 723 "sub_st"
  , H.mkSeq 723 724
  , H.mkVar 724 "res"
  , H.mkSeq 724 725
  , H.mkAssign 725 "value" (Num 0)
  , H.mkSeq 725 726
  , H.mkVar 726 "value_o"
  , H.mkSeq 726 727
  , H.mkBranch 727 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 729 729
  , H.mkSeq 728 42
  , H.mkSeq 728 730
  , H.mkVar 729 "NOP_729"
  , H.mkSeq 729 730
  , H.mkVar 730 "IF_ELSE_FOOTER"
  , H.mkAssign 731 "nos" (Num 0)
  , H.mkSeq 731 732
  , H.mkVar 732 "o"
  , H.mkSeq 732 733
  , H.mkBranch 733 (Eq (Plus (Plus (Num 0) (Plus (Id "PyTuple_Type") (Num 0))) (Num 0)) (Num 1)) 735 735
  , H.mkSeq 734 42
  , H.mkSeq 734 736
  , H.mkVar 735 "NOP_735"
  , H.mkSeq 735 736
  , H.mkVar 736 "IF_ELSE_FOOTER"
  , H.mkAssign 737 "sub_st" (Num 0)
  , H.mkSeq 737 738
  , H.mkAssign 738 "tuple_st" (Num 0)
  , H.mkSeq 738 739
  , H.mkVar 739 "sub"
  , H.mkSeq 739 740
  , H.mkVar 740 "tuple"
  , H.mkSeq 740 741
  , H.mkBranch 741 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 743 743
  , H.mkSeq 742 42
  , H.mkSeq 742 744
  , H.mkVar 743 "NOP_743"
  , H.mkSeq 743 744
  , H.mkVar 744 "IF_ELSE_FOOTER"
  , H.mkVar 745 "index"
  , H.mkSeq 745 746
  , H.mkBranch 746 (Eq (Plus (Id "index") (Num 0)) (Num 1)) 748 748
  , H.mkSeq 747 42
  , H.mkSeq 747 749
  , H.mkVar 748 "NOP_748"
  , H.mkSeq 748 749
  , H.mkVar 749 "IF_ELSE_FOOTER"
  , H.mkVar 750 "res_o"
  , H.mkSeq 750 751
  , H.mkAssign 751 "res" (Num 0)
  , H.mkSeq 751 752
  , H.mkAssign 752 "stack_pointer" (Num 0)
  , H.mkSeq 752 753
  , H.mkVar 753 "tmp"
  , H.mkSeq 753 754
  , H.mkAssign 754 "tuple_st" (Num 0)
  , H.mkSeq 754 755
  , H.mkAssign 755 "undefed" (Num 0)
  , H.mkSeq 755 756
  , H.mkAssign 756 "stack_pointer" (Num 0)
  , H.mkSeq 756 757
  , H.mkVar 757 "word"
  , H.mkSeq 757 758
  , H.mkAssign 758 "opcode" (Num 0)
  , H.mkSeq 758 759
  , H.mkAssign 759 "oparg" (Num 0)
  , H.mkSeq 759 760
  , H.mkBranch 760 (Eq (Num 0) (Num 1)) 761 764
  , H.mkVar 761 "word"
  , H.mkSeq 761 762
  , H.mkAssign 762 "opcode" (Num 0)
  , H.mkSeq 762 763
  , H.mkAssign 763 "oparg" (Num 0)
  , H.mkSeq 763 764
  , H.mkSeq 763 760
  , H.mkVar 764 "LOOP_FOOTER"
  , H.mkSeq 764 765
  , H.mkSeq 764 35
  , H.mkBranch 765 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 767 807
  , H.mkVar 767 "NOP_767"
  , H.mkVar 768 "__CLABEL_TARGET_BINARY_OP_SUBTRACT_FLOAT"
  , H.mkSeq 768 769
  , H.mkVar 769 "this_instr"
  , H.mkSeq 769 770
  , H.mkAssign 770 "undefed" (Num 0)
  , H.mkSeq 770 771
  , H.mkAssign 771 "next_instr" (Num 0)
  , H.mkSeq 771 772
  , H.mkVar 772 "value"
  , H.mkSeq 772 773
  , H.mkVar 773 "left"
  , H.mkSeq 773 774
  , H.mkVar 774 "right"
  , H.mkSeq 774 775
  , H.mkVar 775 "res"
  , H.mkSeq 775 776
  , H.mkAssign 776 "value" (Num 0)
  , H.mkSeq 776 777
  , H.mkVar 777 "value_o"
  , H.mkSeq 777 778
  , H.mkBranch 778 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 780 780
  , H.mkSeq 779 42
  , H.mkSeq 779 781
  , H.mkVar 780 "NOP_780"
  , H.mkSeq 780 781
  , H.mkVar 781 "IF_ELSE_FOOTER"
  , H.mkAssign 782 "left" (Num 0)
  , H.mkSeq 782 783
  , H.mkVar 783 "left_o"
  , H.mkSeq 783 784
  , H.mkBranch 784 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 786 786
  , H.mkSeq 785 42
  , H.mkSeq 785 787
  , H.mkVar 786 "NOP_786"
  , H.mkSeq 786 787
  , H.mkVar 787 "IF_ELSE_FOOTER"
  , H.mkAssign 788 "right" (Num 0)
  , H.mkSeq 788 789
  , H.mkVar 789 "left_o"
  , H.mkSeq 789 790
  , H.mkVar 790 "right_o"
  , H.mkSeq 790 791
  , H.mkVar 791 "dres"
  , H.mkSeq 791 792
  , H.mkAssign 792 "res" (Num 0)
  , H.mkSeq 792 793
  , H.mkBranch 793 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 795 795
  , H.mkSeq 794 1813
  , H.mkSeq 794 796
  , H.mkVar 795 "NOP_795"
  , H.mkSeq 795 796
  , H.mkVar 796 "IF_ELSE_FOOTER"
  , H.mkAssign 797 "undefed" (Num 0)
  , H.mkSeq 797 798
  , H.mkAssign 798 "stack_pointer" (Num 0)
  , H.mkSeq 798 799
  , H.mkVar 799 "word"
  , H.mkSeq 799 800
  , H.mkAssign 800 "opcode" (Num 0)
  , H.mkSeq 800 801
  , H.mkAssign 801 "oparg" (Num 0)
  , H.mkSeq 801 802
  , H.mkBranch 802 (Eq (Num 0) (Num 1)) 803 806
  , H.mkVar 803 "word"
  , H.mkSeq 803 804
  , H.mkAssign 804 "opcode" (Num 0)
  , H.mkSeq 804 805
  , H.mkAssign 805 "oparg" (Num 0)
  , H.mkSeq 805 806
  , H.mkSeq 805 802
  , H.mkVar 806 "LOOP_FOOTER"
  , H.mkSeq 806 807
  , H.mkSeq 806 35
  , H.mkBranch 807 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 809 848
  , H.mkVar 809 "NOP_809"
  , H.mkVar 810 "__CLABEL_TARGET_BINARY_OP_SUBTRACT_INT"
  , H.mkSeq 810 811
  , H.mkVar 811 "this_instr"
  , H.mkSeq 811 812
  , H.mkAssign 812 "undefed" (Num 0)
  , H.mkSeq 812 813
  , H.mkAssign 813 "next_instr" (Num 0)
  , H.mkSeq 813 814
  , H.mkVar 814 "value"
  , H.mkSeq 814 815
  , H.mkVar 815 "left"
  , H.mkSeq 815 816
  , H.mkVar 816 "right"
  , H.mkSeq 816 817
  , H.mkVar 817 "res"
  , H.mkSeq 817 818
  , H.mkAssign 818 "value" (Num 0)
  , H.mkSeq 818 819
  , H.mkVar 819 "value_o"
  , H.mkSeq 819 820
  , H.mkBranch 820 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 822 822
  , H.mkSeq 821 42
  , H.mkSeq 821 823
  , H.mkVar 822 "NOP_822"
  , H.mkSeq 822 823
  , H.mkVar 823 "IF_ELSE_FOOTER"
  , H.mkAssign 824 "left" (Num 0)
  , H.mkSeq 824 825
  , H.mkVar 825 "left_o"
  , H.mkSeq 825 826
  , H.mkBranch 826 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 828 828
  , H.mkSeq 827 42
  , H.mkSeq 827 829
  , H.mkVar 828 "NOP_828"
  , H.mkSeq 828 829
  , H.mkVar 829 "IF_ELSE_FOOTER"
  , H.mkAssign 830 "right" (Num 0)
  , H.mkSeq 830 831
  , H.mkVar 831 "left_o"
  , H.mkSeq 831 832
  , H.mkVar 832 "right_o"
  , H.mkSeq 832 833
  , H.mkAssign 833 "res" (Num 0)
  , H.mkSeq 833 834
  , H.mkBranch 834 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 836 836
  , H.mkSeq 835 42
  , H.mkSeq 835 837
  , H.mkVar 836 "NOP_836"
  , H.mkSeq 836 837
  , H.mkVar 837 "IF_ELSE_FOOTER"
  , H.mkAssign 838 "undefed" (Num 0)
  , H.mkSeq 838 839
  , H.mkAssign 839 "stack_pointer" (Num 0)
  , H.mkSeq 839 840
  , H.mkVar 840 "word"
  , H.mkSeq 840 841
  , H.mkAssign 841 "opcode" (Num 0)
  , H.mkSeq 841 842
  , H.mkAssign 842 "oparg" (Num 0)
  , H.mkSeq 842 843
  , H.mkBranch 843 (Eq (Num 0) (Num 1)) 844 847
  , H.mkVar 844 "word"
  , H.mkSeq 844 845
  , H.mkAssign 845 "opcode" (Num 0)
  , H.mkSeq 845 846
  , H.mkAssign 846 "oparg" (Num 0)
  , H.mkSeq 846 847
  , H.mkSeq 846 843
  , H.mkVar 847 "LOOP_FOOTER"
  , H.mkSeq 847 848
  , H.mkSeq 847 35
  , H.mkBranch 848 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 850 931
  , H.mkVar 850 "NOP_850"
  , H.mkVar 851 "__CLABEL_TARGET_BINARY_SLICE"
  , H.mkSeq 851 852
  , H.mkAssign 852 "undefed" (Num 0)
  , H.mkSeq 852 853
  , H.mkAssign 853 "next_instr" (Num 0)
  , H.mkSeq 853 854
  , H.mkVar 854 "container"
  , H.mkSeq 854 855
  , H.mkVar 855 "start"
  , H.mkSeq 855 856
  , H.mkVar 856 "stop"
  , H.mkSeq 856 857
  , H.mkVar 857 "res"
  , H.mkSeq 857 858
  , H.mkAssign 858 "stop" (Num 0)
  , H.mkSeq 858 859
  , H.mkAssign 859 "start" (Num 0)
  , H.mkSeq 859 860
  , H.mkAssign 860 "container" (Num 0)
  , H.mkSeq 860 861
  , H.mkVar 861 "slice"
  , H.mkSeq 861 862
  , H.mkAssign 862 "stack_pointer" (Num 0)
  , H.mkSeq 862 863
  , H.mkVar 863 "res_o"
  , H.mkSeq 863 864
  , H.mkBranch 864 (Eq (Plus (Id "slice") (Num 0)) (Num 1)) 866 867
  , H.mkAssign 866 "res_o" (Num 0)
  , H.mkSeq 866 913
  , H.mkAssign 867 "stack_pointer" (Num 0)
  , H.mkSeq 867 868
  , H.mkAssign 868 "res_o" (Num 0)
  , H.mkSeq 868 869
  , H.mkVar 869 "op"
  , H.mkSeq 869 870
  , H.mkBranch 870 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 872 887
  , H.mkVar 872 "tracer"
  , H.mkSeq 872 873
  , H.mkBranch 873 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 875 876
  , H.mkVar 875 "data"
  , H.mkSeq 875 876
  , H.mkSeq 875 877
  , H.mkVar 876 "NOP_876"
  , H.mkSeq 876 877
  , H.mkVar 877 "IF_ELSE_FOOTER"
  , H.mkBranch 878 (Eq (Num 0) (Num 1)) 879 885
  , H.mkVar 879 "tracer"
  , H.mkSeq 879 880
  , H.mkBranch 880 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 882 883
  , H.mkVar 882 "data"
  , H.mkSeq 882 883
  , H.mkSeq 882 884
  , H.mkVar 883 "NOP_883"
  , H.mkSeq 883 884
  , H.mkVar 884 "IF_ELSE_FOOTER"
  , H.mkSeq 884 878
  , H.mkVar 885 "LOOP_FOOTER"
  , H.mkSeq 885 886
  , H.mkVar 886 "dealloc"
  , H.mkSeq 886 887
  , H.mkSeq 886 888
  , H.mkVar 887 "NOP_887"
  , H.mkSeq 887 888
  , H.mkVar 888 "IF_ELSE_FOOTER"
  , H.mkBranch 889 (Eq (Num 0) (Num 1)) 890 910
  , H.mkVar 890 "op"
  , H.mkSeq 890 891
  , H.mkBranch 891 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 893 908
  , H.mkVar 893 "tracer"
  , H.mkSeq 893 894
  , H.mkBranch 894 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 896 897
  , H.mkVar 896 "data"
  , H.mkSeq 896 897
  , H.mkSeq 896 898
  , H.mkVar 897 "NOP_897"
  , H.mkSeq 897 898
  , H.mkVar 898 "IF_ELSE_FOOTER"
  , H.mkBranch 899 (Eq (Num 0) (Num 1)) 900 906
  , H.mkVar 900 "tracer"
  , H.mkSeq 900 901
  , H.mkBranch 901 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 903 904
  , H.mkVar 903 "data"
  , H.mkSeq 903 904
  , H.mkSeq 903 905
  , H.mkVar 904 "NOP_904"
  , H.mkSeq 904 905
  , H.mkVar 905 "IF_ELSE_FOOTER"
  , H.mkSeq 905 899
  , H.mkVar 906 "LOOP_FOOTER"
  , H.mkSeq 906 907
  , H.mkVar 907 "dealloc"
  , H.mkSeq 907 908
  , H.mkSeq 907 909
  , H.mkVar 908 "NOP_908"
  , H.mkSeq 908 909
  , H.mkVar 909 "IF_ELSE_FOOTER"
  , H.mkSeq 909 889
  , H.mkVar 910 "LOOP_FOOTER"
  , H.mkSeq 910 911
  , H.mkAssign 911 "stack_pointer" (Num 0)
  , H.mkSeq 911 912
  , H.mkAssign 912 "stack_pointer" (Num 0)
  , H.mkSeq 912 913
  , H.mkVar 913 "IF_ELSE_FOOTER"
  , H.mkAssign 914 "stack_pointer" (Num 0)
  , H.mkSeq 914 915
  , H.mkAssign 915 "stack_pointer" (Num 0)
  , H.mkSeq 915 916
  , H.mkBranch 916 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 918 918
  , H.mkSeq 917 1817
  , H.mkSeq 917 919
  , H.mkVar 918 "NOP_918"
  , H.mkSeq 918 919
  , H.mkVar 919 "IF_ELSE_FOOTER"
  , H.mkAssign 920 "res" (Num 0)
  , H.mkSeq 920 921
  , H.mkAssign 921 "undefed" (Num 0)
  , H.mkSeq 921 922
  , H.mkAssign 922 "stack_pointer" (Num 0)
  , H.mkSeq 922 923
  , H.mkVar 923 "word"
  , H.mkSeq 923 924
  , H.mkAssign 924 "opcode" (Num 0)
  , H.mkSeq 924 925
  , H.mkAssign 925 "oparg" (Num 0)
  , H.mkSeq 925 926
  , H.mkBranch 926 (Eq (Num 0) (Num 1)) 927 930
  , H.mkVar 927 "word"
  , H.mkSeq 927 928
  , H.mkAssign 928 "opcode" (Num 0)
  , H.mkSeq 928 929
  , H.mkAssign 929 "oparg" (Num 0)
  , H.mkSeq 929 930
  , H.mkSeq 929 926
  , H.mkVar 930 "LOOP_FOOTER"
  , H.mkSeq 930 931
  , H.mkSeq 930 35
  , H.mkBranch 931 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 933 980
  , H.mkVar 933 "NOP_933"
  , H.mkVar 934 "__CLABEL_TARGET_BUILD_INTERPOLATION"
  , H.mkSeq 934 935
  , H.mkAssign 935 "undefed" (Num 0)
  , H.mkSeq 935 936
  , H.mkAssign 936 "next_instr" (Num 0)
  , H.mkSeq 936 937
  , H.mkVar 937 "value"
  , H.mkSeq 937 938
  , H.mkVar 938 "str"
  , H.mkSeq 938 939
  , H.mkVar 939 "format"
  , H.mkSeq 939 940
  , H.mkVar 940 "interpolation"
  , H.mkSeq 940 941
  , H.mkAssign 941 "format" (Num 0)
  , H.mkSeq 941 942
  , H.mkAssign 942 "str" (Num 0)
  , H.mkSeq 942 943
  , H.mkAssign 943 "value" (Num 0)
  , H.mkSeq 943 944
  , H.mkVar 944 "value_o"
  , H.mkSeq 944 945
  , H.mkVar 945 "str_o"
  , H.mkSeq 945 946
  , H.mkVar 946 "conversion"
  , H.mkSeq 946 947
  , H.mkVar 947 "format_o"
  , H.mkSeq 947 948
  , H.mkBranch 948 (Eq (Plus (Id "oparg") (Num 0)) (Num 1)) 950 951
  , H.mkAssign 950 "format_o" (Num 0)
  , H.mkSeq 950 952
  , H.mkAssign 951 "format_o" (Num 0)
  , H.mkSeq 951 952
  , H.mkVar 952 "IF_ELSE_FOOTER"
  , H.mkVar 953 "interpolation_o"
  , H.mkSeq 953 954
  , H.mkAssign 954 "stack_pointer" (Num 0)
  , H.mkSeq 954 955
  , H.mkBranch 955 (Eq (Plus (Id "oparg") (Num 0)) (Num 1)) 957 959
  , H.mkAssign 957 "stack_pointer" (Num 0)
  , H.mkSeq 957 958
  , H.mkAssign 958 "stack_pointer" (Num 0)
  , H.mkSeq 958 959
  , H.mkSeq 958 960
  , H.mkAssign 959 "stack_pointer" (Num 0)
  , H.mkSeq 959 960
  , H.mkVar 960 "IF_ELSE_FOOTER"
  , H.mkAssign 961 "stack_pointer" (Num 0)
  , H.mkSeq 961 962
  , H.mkAssign 962 "stack_pointer" (Num 0)
  , H.mkSeq 962 963
  , H.mkAssign 963 "stack_pointer" (Num 0)
  , H.mkSeq 963 964
  , H.mkAssign 964 "stack_pointer" (Num 0)
  , H.mkSeq 964 965
  , H.mkBranch 965 (Eq (Plus (Id "interpolation_o") (Num 0)) (Num 1)) 967 967
  , H.mkSeq 966 1817
  , H.mkSeq 966 968
  , H.mkVar 967 "NOP_967"
  , H.mkSeq 967 968
  , H.mkVar 968 "IF_ELSE_FOOTER"
  , H.mkAssign 969 "interpolation" (Num 0)
  , H.mkSeq 969 970
  , H.mkAssign 970 "undefed" (Num 0)
  , H.mkSeq 970 971
  , H.mkAssign 971 "stack_pointer" (Num 0)
  , H.mkSeq 971 972
  , H.mkVar 972 "word"
  , H.mkSeq 972 973
  , H.mkAssign 973 "opcode" (Num 0)
  , H.mkSeq 973 974
  , H.mkAssign 974 "oparg" (Num 0)
  , H.mkSeq 974 975
  , H.mkBranch 975 (Eq (Num 0) (Num 1)) 976 979
  , H.mkVar 976 "word"
  , H.mkSeq 976 977
  , H.mkAssign 977 "opcode" (Num 0)
  , H.mkSeq 977 978
  , H.mkAssign 978 "oparg" (Num 0)
  , H.mkSeq 978 979
  , H.mkSeq 978 975
  , H.mkVar 979 "LOOP_FOOTER"
  , H.mkSeq 979 980
  , H.mkSeq 979 35
  , H.mkBranch 980 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 982 1006
  , H.mkVar 982 "NOP_982"
  , H.mkVar 983 "__CLABEL_TARGET_BUILD_LIST"
  , H.mkSeq 983 984
  , H.mkAssign 984 "undefed" (Num 0)
  , H.mkSeq 984 985
  , H.mkAssign 985 "next_instr" (Num 0)
  , H.mkSeq 985 986
  , H.mkVar 986 "values"
  , H.mkSeq 986 987
  , H.mkVar 987 "list"
  , H.mkSeq 987 988
  , H.mkAssign 988 "values" (Num 0)
  , H.mkSeq 988 989
  , H.mkVar 989 "list_o"
  , H.mkSeq 989 990
  , H.mkAssign 990 "stack_pointer" (Num 0)
  , H.mkSeq 990 991
  , H.mkBranch 991 (Eq (Plus (Id "list_o") (Num 0)) (Num 1)) 993 993
  , H.mkSeq 992 1817
  , H.mkSeq 992 994
  , H.mkVar 993 "NOP_993"
  , H.mkSeq 993 994
  , H.mkVar 994 "IF_ELSE_FOOTER"
  , H.mkAssign 995 "list" (Num 0)
  , H.mkSeq 995 996
  , H.mkAssign 996 "undefed" (Num 0)
  , H.mkSeq 996 997
  , H.mkAssign 997 "stack_pointer" (Num 0)
  , H.mkSeq 997 998
  , H.mkVar 998 "word"
  , H.mkSeq 998 999
  , H.mkAssign 999 "opcode" (Num 0)
  , H.mkSeq 999 1000
  , H.mkAssign 1000 "oparg" (Num 0)
  , H.mkSeq 1000 1001
  , H.mkBranch 1001 (Eq (Num 0) (Num 1)) 1002 1005
  , H.mkVar 1002 "word"
  , H.mkSeq 1002 1003
  , H.mkAssign 1003 "opcode" (Num 0)
  , H.mkSeq 1003 1004
  , H.mkAssign 1004 "oparg" (Num 0)
  , H.mkSeq 1004 1005
  , H.mkSeq 1004 1001
  , H.mkVar 1005 "LOOP_FOOTER"
  , H.mkSeq 1005 1006
  , H.mkSeq 1005 35
  , H.mkBranch 1006 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1008 1054
  , H.mkVar 1008 "NOP_1008"
  , H.mkVar 1009 "__CLABEL_TARGET_BUILD_MAP"
  , H.mkSeq 1009 1010
  , H.mkAssign 1010 "undefed" (Num 0)
  , H.mkSeq 1010 1011
  , H.mkAssign 1011 "next_instr" (Num 0)
  , H.mkSeq 1011 1012
  , H.mkVar 1012 "values"
  , H.mkSeq 1012 1013
  , H.mkVar 1013 "map"
  , H.mkSeq 1013 1014
  , H.mkAssign 1014 "values" (Num 0)
  , H.mkSeq 1014 1015
  , H.mkVar 1015 "values_o_temp"
  , H.mkSeq 1015 1016
  , H.mkVar 1016 "values_o"
  , H.mkSeq 1016 1017
  , H.mkBranch 1017 (Eq (Plus (Id "values_o") (Num 0)) (Num 1)) 1019 1027
  , H.mkVar 1019 "tmp"
  , H.mkSeq 1019 1020
  , H.mkVar 1020 "_i"
  , H.mkSeq 1020 1021
  , H.mkBranch 1021 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1022 1024
  , H.mkAssign 1022 "tmp" (Num 0)
  , H.mkSeq 1022 1023
  , H.mkAssign 1023 "undefed" (Num 0)
  , H.mkSeq 1023 1024
  , H.mkSeq 1023 1021
  , H.mkVar 1024 "LOOP_FOOTER"
  , H.mkSeq 1024 1025
  , H.mkAssign 1025 "stack_pointer" (Num 0)
  , H.mkSeq 1025 1026
  , H.mkAssign 1026 "stack_pointer" (Num 0)
  , H.mkSeq 1026 1027
  , H.mkSeq 1026 1817
  , H.mkSeq 1026 1028
  , H.mkVar 1027 "NOP_1027"
  , H.mkSeq 1027 1028
  , H.mkVar 1028 "IF_ELSE_FOOTER"
  , H.mkVar 1029 "map_o"
  , H.mkSeq 1029 1030
  , H.mkAssign 1030 "stack_pointer" (Num 0)
  , H.mkSeq 1030 1031
  , H.mkVar 1031 "tmp"
  , H.mkSeq 1031 1032
  , H.mkVar 1032 "_i"
  , H.mkSeq 1032 1033
  , H.mkBranch 1033 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1034 1036
  , H.mkAssign 1034 "tmp" (Num 0)
  , H.mkSeq 1034 1035
  , H.mkAssign 1035 "undefed" (Num 0)
  , H.mkSeq 1035 1036
  , H.mkSeq 1035 1033
  , H.mkVar 1036 "LOOP_FOOTER"
  , H.mkSeq 1036 1037
  , H.mkAssign 1037 "stack_pointer" (Num 0)
  , H.mkSeq 1037 1038
  , H.mkAssign 1038 "stack_pointer" (Num 0)
  , H.mkSeq 1038 1039
  , H.mkBranch 1039 (Eq (Plus (Id "map_o") (Num 0)) (Num 1)) 1041 1041
  , H.mkSeq 1040 1817
  , H.mkSeq 1040 1042
  , H.mkVar 1041 "NOP_1041"
  , H.mkSeq 1041 1042
  , H.mkVar 1042 "IF_ELSE_FOOTER"
  , H.mkAssign 1043 "map" (Num 0)
  , H.mkSeq 1043 1044
  , H.mkAssign 1044 "undefed" (Num 0)
  , H.mkSeq 1044 1045
  , H.mkAssign 1045 "stack_pointer" (Num 0)
  , H.mkSeq 1045 1046
  , H.mkVar 1046 "word"
  , H.mkSeq 1046 1047
  , H.mkAssign 1047 "opcode" (Num 0)
  , H.mkSeq 1047 1048
  , H.mkAssign 1048 "oparg" (Num 0)
  , H.mkSeq 1048 1049
  , H.mkBranch 1049 (Eq (Num 0) (Num 1)) 1050 1053
  , H.mkVar 1050 "word"
  , H.mkSeq 1050 1051
  , H.mkAssign 1051 "opcode" (Num 0)
  , H.mkSeq 1051 1052
  , H.mkAssign 1052 "oparg" (Num 0)
  , H.mkSeq 1052 1053
  , H.mkSeq 1052 1049
  , H.mkVar 1053 "LOOP_FOOTER"
  , H.mkSeq 1053 1054
  , H.mkSeq 1053 35
  , H.mkBranch 1054 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1056 1148
  , H.mkVar 1056 "NOP_1056"
  , H.mkVar 1057 "__CLABEL_TARGET_BUILD_SET"
  , H.mkSeq 1057 1058
  , H.mkAssign 1058 "undefed" (Num 0)
  , H.mkSeq 1058 1059
  , H.mkAssign 1059 "next_instr" (Num 0)
  , H.mkSeq 1059 1060
  , H.mkVar 1060 "values"
  , H.mkSeq 1060 1061
  , H.mkVar 1061 "set"
  , H.mkSeq 1061 1062
  , H.mkAssign 1062 "values" (Num 0)
  , H.mkSeq 1062 1063
  , H.mkVar 1063 "set_o"
  , H.mkSeq 1063 1064
  , H.mkAssign 1064 "stack_pointer" (Num 0)
  , H.mkSeq 1064 1065
  , H.mkBranch 1065 (Eq (Plus (Id "set_o") (Num 0)) (Num 1)) 1067 1075
  , H.mkVar 1067 "tmp"
  , H.mkSeq 1067 1068
  , H.mkVar 1068 "_i"
  , H.mkSeq 1068 1069
  , H.mkBranch 1069 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1070 1072
  , H.mkAssign 1070 "tmp" (Num 0)
  , H.mkSeq 1070 1071
  , H.mkAssign 1071 "undefed" (Num 0)
  , H.mkSeq 1071 1072
  , H.mkSeq 1071 1069
  , H.mkVar 1072 "LOOP_FOOTER"
  , H.mkSeq 1072 1073
  , H.mkAssign 1073 "stack_pointer" (Num 0)
  , H.mkSeq 1073 1074
  , H.mkAssign 1074 "stack_pointer" (Num 0)
  , H.mkSeq 1074 1075
  , H.mkSeq 1074 1817
  , H.mkSeq 1074 1076
  , H.mkVar 1075 "NOP_1075"
  , H.mkSeq 1075 1076
  , H.mkVar 1076 "IF_ELSE_FOOTER"
  , H.mkVar 1077 "err"
  , H.mkSeq 1077 1078
  , H.mkVar 1078 "i"
  , H.mkSeq 1078 1079
  , H.mkBranch 1079 (Eq (Plus (Id "i") (Id "oparg")) (Num 1)) 1080 1088
  , H.mkVar 1080 "value"
  , H.mkSeq 1080 1081
  , H.mkAssign 1081 "undefed" (Num 0)
  , H.mkSeq 1081 1082
  , H.mkBranch 1082 (Eq (Plus (Id "err") (Num 0)) (Num 1)) 1084 1086
  , H.mkAssign 1084 "err" (Num 0)
  , H.mkSeq 1084 1085
  , H.mkAssign 1085 "stack_pointer" (Num 0)
  , H.mkSeq 1085 1086
  , H.mkSeq 1085 1087
  , H.mkAssign 1086 "stack_pointer" (Num 0)
  , H.mkSeq 1086 1087
  , H.mkVar 1087 "IF_ELSE_FOOTER"
  , H.mkSeq 1087 1079
  , H.mkVar 1088 "LOOP_FOOTER"
  , H.mkSeq 1088 1089
  , H.mkBranch 1089 (Eq (Id "err") (Num 1)) 1091 1135
  , H.mkAssign 1091 "stack_pointer" (Num 0)
  , H.mkSeq 1091 1092
  , H.mkVar 1092 "op"
  , H.mkSeq 1092 1093
  , H.mkBranch 1093 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1095 1110
  , H.mkVar 1095 "tracer"
  , H.mkSeq 1095 1096
  , H.mkBranch 1096 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1098 1099
  , H.mkVar 1098 "data"
  , H.mkSeq 1098 1099
  , H.mkSeq 1098 1100
  , H.mkVar 1099 "NOP_1099"
  , H.mkSeq 1099 1100
  , H.mkVar 1100 "IF_ELSE_FOOTER"
  , H.mkBranch 1101 (Eq (Num 0) (Num 1)) 1102 1108
  , H.mkVar 1102 "tracer"
  , H.mkSeq 1102 1103
  , H.mkBranch 1103 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1105 1106
  , H.mkVar 1105 "data"
  , H.mkSeq 1105 1106
  , H.mkSeq 1105 1107
  , H.mkVar 1106 "NOP_1106"
  , H.mkSeq 1106 1107
  , H.mkVar 1107 "IF_ELSE_FOOTER"
  , H.mkSeq 1107 1101
  , H.mkVar 1108 "LOOP_FOOTER"
  , H.mkSeq 1108 1109
  , H.mkVar 1109 "dealloc"
  , H.mkSeq 1109 1110
  , H.mkSeq 1109 1111
  , H.mkVar 1110 "NOP_1110"
  , H.mkSeq 1110 1111
  , H.mkVar 1111 "IF_ELSE_FOOTER"
  , H.mkBranch 1112 (Eq (Num 0) (Num 1)) 1113 1133
  , H.mkVar 1113 "op"
  , H.mkSeq 1113 1114
  , H.mkBranch 1114 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1116 1131
  , H.mkVar 1116 "tracer"
  , H.mkSeq 1116 1117
  , H.mkBranch 1117 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1119 1120
  , H.mkVar 1119 "data"
  , H.mkSeq 1119 1120
  , H.mkSeq 1119 1121
  , H.mkVar 1120 "NOP_1120"
  , H.mkSeq 1120 1121
  , H.mkVar 1121 "IF_ELSE_FOOTER"
  , H.mkBranch 1122 (Eq (Num 0) (Num 1)) 1123 1129
  , H.mkVar 1123 "tracer"
  , H.mkSeq 1123 1124
  , H.mkBranch 1124 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1126 1127
  , H.mkVar 1126 "data"
  , H.mkSeq 1126 1127
  , H.mkSeq 1126 1128
  , H.mkVar 1127 "NOP_1127"
  , H.mkSeq 1127 1128
  , H.mkVar 1128 "IF_ELSE_FOOTER"
  , H.mkSeq 1128 1122
  , H.mkVar 1129 "LOOP_FOOTER"
  , H.mkSeq 1129 1130
  , H.mkVar 1130 "dealloc"
  , H.mkSeq 1130 1131
  , H.mkSeq 1130 1132
  , H.mkVar 1131 "NOP_1131"
  , H.mkSeq 1131 1132
  , H.mkVar 1132 "IF_ELSE_FOOTER"
  , H.mkSeq 1132 1112
  , H.mkVar 1133 "LOOP_FOOTER"
  , H.mkSeq 1133 1134
  , H.mkAssign 1134 "stack_pointer" (Num 0)
  , H.mkSeq 1134 1135
  , H.mkSeq 1134 1817
  , H.mkSeq 1134 1136
  , H.mkVar 1135 "NOP_1135"
  , H.mkSeq 1135 1136
  , H.mkVar 1136 "IF_ELSE_FOOTER"
  , H.mkAssign 1137 "set" (Num 0)
  , H.mkSeq 1137 1138
  , H.mkAssign 1138 "undefed" (Num 0)
  , H.mkSeq 1138 1139
  , H.mkAssign 1139 "stack_pointer" (Num 0)
  , H.mkSeq 1139 1140
  , H.mkVar 1140 "word"
  , H.mkSeq 1140 1141
  , H.mkAssign 1141 "opcode" (Num 0)
  , H.mkSeq 1141 1142
  , H.mkAssign 1142 "oparg" (Num 0)
  , H.mkSeq 1142 1143
  , H.mkBranch 1143 (Eq (Num 0) (Num 1)) 1144 1147
  , H.mkVar 1144 "word"
  , H.mkSeq 1144 1145
  , H.mkAssign 1145 "opcode" (Num 0)
  , H.mkSeq 1145 1146
  , H.mkAssign 1146 "oparg" (Num 0)
  , H.mkSeq 1146 1147
  , H.mkSeq 1146 1143
  , H.mkVar 1147 "LOOP_FOOTER"
  , H.mkSeq 1147 1148
  , H.mkSeq 1147 35
  , H.mkBranch 1148 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1150 1184
  , H.mkVar 1150 "NOP_1150"
  , H.mkVar 1151 "__CLABEL_TARGET_BUILD_SLICE"
  , H.mkSeq 1151 1152
  , H.mkAssign 1152 "undefed" (Num 0)
  , H.mkSeq 1152 1153
  , H.mkAssign 1153 "next_instr" (Num 0)
  , H.mkSeq 1153 1154
  , H.mkVar 1154 "args"
  , H.mkSeq 1154 1155
  , H.mkVar 1155 "slice"
  , H.mkSeq 1155 1156
  , H.mkAssign 1156 "args" (Num 0)
  , H.mkSeq 1156 1157
  , H.mkVar 1157 "start_o"
  , H.mkSeq 1157 1158
  , H.mkVar 1158 "stop_o"
  , H.mkSeq 1158 1159
  , H.mkVar 1159 "step_o"
  , H.mkSeq 1159 1160
  , H.mkVar 1160 "slice_o"
  , H.mkSeq 1160 1161
  , H.mkVar 1161 "tmp"
  , H.mkSeq 1161 1162
  , H.mkVar 1162 "_i"
  , H.mkSeq 1162 1163
  , H.mkBranch 1163 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1164 1166
  , H.mkAssign 1164 "tmp" (Num 0)
  , H.mkSeq 1164 1165
  , H.mkAssign 1165 "undefed" (Num 0)
  , H.mkSeq 1165 1166
  , H.mkSeq 1165 1163
  , H.mkVar 1166 "LOOP_FOOTER"
  , H.mkSeq 1166 1167
  , H.mkAssign 1167 "stack_pointer" (Num 0)
  , H.mkSeq 1167 1168
  , H.mkAssign 1168 "stack_pointer" (Num 0)
  , H.mkSeq 1168 1169
  , H.mkBranch 1169 (Eq (Plus (Id "slice_o") (Num 0)) (Num 1)) 1171 1171
  , H.mkSeq 1170 1817
  , H.mkSeq 1170 1172
  , H.mkVar 1171 "NOP_1171"
  , H.mkSeq 1171 1172
  , H.mkVar 1172 "IF_ELSE_FOOTER"
  , H.mkAssign 1173 "slice" (Num 0)
  , H.mkSeq 1173 1174
  , H.mkAssign 1174 "undefed" (Num 0)
  , H.mkSeq 1174 1175
  , H.mkAssign 1175 "stack_pointer" (Num 0)
  , H.mkSeq 1175 1176
  , H.mkVar 1176 "word"
  , H.mkSeq 1176 1177
  , H.mkAssign 1177 "opcode" (Num 0)
  , H.mkSeq 1177 1178
  , H.mkAssign 1178 "oparg" (Num 0)
  , H.mkSeq 1178 1179
  , H.mkBranch 1179 (Eq (Num 0) (Num 1)) 1180 1183
  , H.mkVar 1180 "word"
  , H.mkSeq 1180 1181
  , H.mkAssign 1181 "opcode" (Num 0)
  , H.mkSeq 1181 1182
  , H.mkAssign 1182 "oparg" (Num 0)
  , H.mkSeq 1182 1183
  , H.mkSeq 1182 1179
  , H.mkVar 1183 "LOOP_FOOTER"
  , H.mkSeq 1183 1184
  , H.mkSeq 1183 35
  , H.mkBranch 1184 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1186 1231
  , H.mkVar 1186 "NOP_1186"
  , H.mkVar 1187 "__CLABEL_TARGET_BUILD_STRING"
  , H.mkSeq 1187 1188
  , H.mkAssign 1188 "undefed" (Num 0)
  , H.mkSeq 1188 1189
  , H.mkAssign 1189 "next_instr" (Num 0)
  , H.mkSeq 1189 1190
  , H.mkVar 1190 "pieces"
  , H.mkSeq 1190 1191
  , H.mkVar 1191 "str"
  , H.mkSeq 1191 1192
  , H.mkAssign 1192 "pieces" (Num 0)
  , H.mkSeq 1192 1193
  , H.mkVar 1193 "pieces_o_temp"
  , H.mkSeq 1193 1194
  , H.mkVar 1194 "pieces_o"
  , H.mkSeq 1194 1195
  , H.mkBranch 1195 (Eq (Plus (Id "pieces_o") (Num 0)) (Num 1)) 1197 1205
  , H.mkVar 1197 "tmp"
  , H.mkSeq 1197 1198
  , H.mkVar 1198 "_i"
  , H.mkSeq 1198 1199
  , H.mkBranch 1199 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1200 1202
  , H.mkAssign 1200 "tmp" (Num 0)
  , H.mkSeq 1200 1201
  , H.mkAssign 1201 "undefed" (Num 0)
  , H.mkSeq 1201 1202
  , H.mkSeq 1201 1199
  , H.mkVar 1202 "LOOP_FOOTER"
  , H.mkSeq 1202 1203
  , H.mkAssign 1203 "stack_pointer" (Num 0)
  , H.mkSeq 1203 1204
  , H.mkAssign 1204 "stack_pointer" (Num 0)
  , H.mkSeq 1204 1205
  , H.mkSeq 1204 1817
  , H.mkSeq 1204 1206
  , H.mkVar 1205 "NOP_1205"
  , H.mkSeq 1205 1206
  , H.mkVar 1206 "IF_ELSE_FOOTER"
  , H.mkVar 1207 "str_o"
  , H.mkSeq 1207 1208
  , H.mkVar 1208 "tmp"
  , H.mkSeq 1208 1209
  , H.mkVar 1209 "_i"
  , H.mkSeq 1209 1210
  , H.mkBranch 1210 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1211 1213
  , H.mkAssign 1211 "tmp" (Num 0)
  , H.mkSeq 1211 1212
  , H.mkAssign 1212 "undefed" (Num 0)
  , H.mkSeq 1212 1213
  , H.mkSeq 1212 1210
  , H.mkVar 1213 "LOOP_FOOTER"
  , H.mkSeq 1213 1214
  , H.mkAssign 1214 "stack_pointer" (Num 0)
  , H.mkSeq 1214 1215
  , H.mkAssign 1215 "stack_pointer" (Num 0)
  , H.mkSeq 1215 1216
  , H.mkBranch 1216 (Eq (Plus (Id "str_o") (Num 0)) (Num 1)) 1218 1218
  , H.mkSeq 1217 1817
  , H.mkSeq 1217 1219
  , H.mkVar 1218 "NOP_1218"
  , H.mkSeq 1218 1219
  , H.mkVar 1219 "IF_ELSE_FOOTER"
  , H.mkAssign 1220 "str" (Num 0)
  , H.mkSeq 1220 1221
  , H.mkAssign 1221 "undefed" (Num 0)
  , H.mkSeq 1221 1222
  , H.mkAssign 1222 "stack_pointer" (Num 0)
  , H.mkSeq 1222 1223
  , H.mkVar 1223 "word"
  , H.mkSeq 1223 1224
  , H.mkAssign 1224 "opcode" (Num 0)
  , H.mkSeq 1224 1225
  , H.mkAssign 1225 "oparg" (Num 0)
  , H.mkSeq 1225 1226
  , H.mkBranch 1226 (Eq (Num 0) (Num 1)) 1227 1230
  , H.mkVar 1227 "word"
  , H.mkSeq 1227 1228
  , H.mkAssign 1228 "opcode" (Num 0)
  , H.mkSeq 1228 1229
  , H.mkAssign 1229 "oparg" (Num 0)
  , H.mkSeq 1229 1230
  , H.mkSeq 1229 1226
  , H.mkVar 1230 "LOOP_FOOTER"
  , H.mkSeq 1230 1231
  , H.mkSeq 1230 35
  , H.mkBranch 1231 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1233 1265
  , H.mkVar 1233 "NOP_1233"
  , H.mkVar 1234 "__CLABEL_TARGET_BUILD_TEMPLATE"
  , H.mkSeq 1234 1235
  , H.mkAssign 1235 "undefed" (Num 0)
  , H.mkSeq 1235 1236
  , H.mkAssign 1236 "next_instr" (Num 0)
  , H.mkSeq 1236 1237
  , H.mkVar 1237 "strings"
  , H.mkSeq 1237 1238
  , H.mkVar 1238 "interpolations"
  , H.mkSeq 1238 1239
  , H.mkVar 1239 "template"
  , H.mkSeq 1239 1240
  , H.mkAssign 1240 "interpolations" (Num 0)
  , H.mkSeq 1240 1241
  , H.mkAssign 1241 "strings" (Num 0)
  , H.mkSeq 1241 1242
  , H.mkVar 1242 "strings_o"
  , H.mkSeq 1242 1243
  , H.mkVar 1243 "interpolations_o"
  , H.mkSeq 1243 1244
  , H.mkVar 1244 "template_o"
  , H.mkSeq 1244 1245
  , H.mkAssign 1245 "stack_pointer" (Num 0)
  , H.mkSeq 1245 1246
  , H.mkAssign 1246 "stack_pointer" (Num 0)
  , H.mkSeq 1246 1247
  , H.mkAssign 1247 "stack_pointer" (Num 0)
  , H.mkSeq 1247 1248
  , H.mkAssign 1248 "stack_pointer" (Num 0)
  , H.mkSeq 1248 1249
  , H.mkAssign 1249 "stack_pointer" (Num 0)
  , H.mkSeq 1249 1250
  , H.mkBranch 1250 (Eq (Plus (Id "template_o") (Num 0)) (Num 1)) 1252 1252
  , H.mkSeq 1251 1817
  , H.mkSeq 1251 1253
  , H.mkVar 1252 "NOP_1252"
  , H.mkSeq 1252 1253
  , H.mkVar 1253 "IF_ELSE_FOOTER"
  , H.mkAssign 1254 "template" (Num 0)
  , H.mkSeq 1254 1255
  , H.mkAssign 1255 "undefed" (Num 0)
  , H.mkSeq 1255 1256
  , H.mkAssign 1256 "stack_pointer" (Num 0)
  , H.mkSeq 1256 1257
  , H.mkVar 1257 "word"
  , H.mkSeq 1257 1258
  , H.mkAssign 1258 "opcode" (Num 0)
  , H.mkSeq 1258 1259
  , H.mkAssign 1259 "oparg" (Num 0)
  , H.mkSeq 1259 1260
  , H.mkBranch 1260 (Eq (Num 0) (Num 1)) 1261 1264
  , H.mkVar 1261 "word"
  , H.mkSeq 1261 1262
  , H.mkAssign 1262 "opcode" (Num 0)
  , H.mkSeq 1262 1263
  , H.mkAssign 1263 "oparg" (Num 0)
  , H.mkSeq 1263 1264
  , H.mkSeq 1263 1260
  , H.mkVar 1264 "LOOP_FOOTER"
  , H.mkSeq 1264 1265
  , H.mkSeq 1264 35
  , H.mkBranch 1265 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1267 1290
  , H.mkVar 1267 "NOP_1267"
  , H.mkVar 1268 "__CLABEL_TARGET_BUILD_TUPLE"
  , H.mkSeq 1268 1269
  , H.mkAssign 1269 "undefed" (Num 0)
  , H.mkSeq 1269 1270
  , H.mkAssign 1270 "next_instr" (Num 0)
  , H.mkSeq 1270 1271
  , H.mkVar 1271 "values"
  , H.mkSeq 1271 1272
  , H.mkVar 1272 "tup"
  , H.mkSeq 1272 1273
  , H.mkAssign 1273 "values" (Num 0)
  , H.mkSeq 1273 1274
  , H.mkVar 1274 "tup_o"
  , H.mkSeq 1274 1275
  , H.mkBranch 1275 (Eq (Plus (Id "tup_o") (Num 0)) (Num 1)) 1277 1277
  , H.mkSeq 1276 1817
  , H.mkSeq 1276 1278
  , H.mkVar 1277 "NOP_1277"
  , H.mkSeq 1277 1278
  , H.mkVar 1278 "IF_ELSE_FOOTER"
  , H.mkAssign 1279 "tup" (Num 0)
  , H.mkSeq 1279 1280
  , H.mkAssign 1280 "undefed" (Num 0)
  , H.mkSeq 1280 1281
  , H.mkAssign 1281 "stack_pointer" (Num 0)
  , H.mkSeq 1281 1282
  , H.mkVar 1282 "word"
  , H.mkSeq 1282 1283
  , H.mkAssign 1283 "opcode" (Num 0)
  , H.mkSeq 1283 1284
  , H.mkAssign 1284 "oparg" (Num 0)
  , H.mkSeq 1284 1285
  , H.mkBranch 1285 (Eq (Num 0) (Num 1)) 1286 1289
  , H.mkVar 1286 "word"
  , H.mkSeq 1286 1287
  , H.mkAssign 1287 "opcode" (Num 0)
  , H.mkSeq 1287 1288
  , H.mkAssign 1288 "oparg" (Num 0)
  , H.mkSeq 1288 1289
  , H.mkSeq 1288 1285
  , H.mkVar 1289 "LOOP_FOOTER"
  , H.mkSeq 1289 1290
  , H.mkSeq 1289 35
  , H.mkBranch 1290 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1292 1304
  , H.mkVar 1292 "NOP_1292"
  , H.mkVar 1293 "__CLABEL_TARGET_CACHE"
  , H.mkSeq 1293 1294
  , H.mkAssign 1294 "undefed" (Num 0)
  , H.mkSeq 1294 1295
  , H.mkAssign 1295 "next_instr" (Num 0)
  , H.mkSeq 1295 1296
  , H.mkVar 1296 "word"
  , H.mkSeq 1296 1297
  , H.mkAssign 1297 "opcode" (Num 0)
  , H.mkSeq 1297 1298
  , H.mkAssign 1298 "oparg" (Num 0)
  , H.mkSeq 1298 1299
  , H.mkBranch 1299 (Eq (Num 0) (Num 1)) 1300 1303
  , H.mkVar 1300 "word"
  , H.mkSeq 1300 1301
  , H.mkAssign 1301 "opcode" (Num 0)
  , H.mkSeq 1301 1302
  , H.mkAssign 1302 "oparg" (Num 0)
  , H.mkSeq 1302 1303
  , H.mkSeq 1302 1299
  , H.mkVar 1303 "LOOP_FOOTER"
  , H.mkSeq 1303 1304
  , H.mkSeq 1303 35
  , H.mkBranch 1304 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1306 1551
  , H.mkVar 1306 "NOP_1306"
  , H.mkVar 1307 "__CLABEL_TARGET_CALL"
  , H.mkSeq 1307 1308
  , H.mkAssign 1308 "undefed" (Num 0)
  , H.mkSeq 1308 1309
  , H.mkAssign 1309 "next_instr" (Num 0)
  , H.mkSeq 1309 1310
  , H.mkVar 1310 "__CLABEL_PREDICTED_CALL"
  , H.mkSeq 1310 1311
  , H.mkVar 1311 "NOP_1311"
  , H.mkVar 1312 "this_instr"
  , H.mkSeq 1312 1313
  , H.mkAssign 1313 "opcode" (Num 0)
  , H.mkSeq 1313 1314
  , H.mkVar 1314 "callable"
  , H.mkSeq 1314 1315
  , H.mkVar 1315 "self_or_null"
  , H.mkSeq 1315 1316
  , H.mkVar 1316 "args"
  , H.mkSeq 1316 1317
  , H.mkVar 1317 "res"
  , H.mkSeq 1317 1318
  , H.mkAssign 1318 "self_or_null" (Num 0)
  , H.mkSeq 1318 1319
  , H.mkAssign 1319 "callable" (Num 0)
  , H.mkSeq 1319 1320
  , H.mkVar 1320 "counter"
  , H.mkSeq 1320 1321
  , H.mkBranch 1321 (Eq (Num 0) (Num 1)) 1323 1326
  , H.mkAssign 1323 "next_instr" (Num 0)
  , H.mkSeq 1323 1324
  , H.mkAssign 1324 "stack_pointer" (Num 0)
  , H.mkSeq 1324 1325
  , H.mkAssign 1325 "opcode" (Num 0)
  , H.mkSeq 1325 1326
  , H.mkSeq 1325 35
  , H.mkSeq 1325 1327
  , H.mkVar 1326 "NOP_1326"
  , H.mkSeq 1326 1327
  , H.mkVar 1327 "IF_ELSE_FOOTER"
  , H.mkAssign 1328 "undefed" (Num 0)
  , H.mkSeq 1328 1329
  , H.mkBranch 1329 (Eq (Num 0) (Num 1)) 1330 1331
  , H.mkAssign 1330 "undefed" (Num 0)
  , H.mkSeq 1330 1329
  , H.mkVar 1331 "LOOP_FOOTER"
  , H.mkSeq 1331 1332
  , H.mkBranch 1332 (Eq (Plus (Plus (Num 0) (Plus (Id "PyMethod_Type") (Num 0))) (Plus (Num 0) (Num 0))) (Num 1)) 1334 1343
  , H.mkVar 1334 "callable_o"
  , H.mkSeq 1334 1335
  , H.mkVar 1335 "self"
  , H.mkSeq 1335 1336
  , H.mkAssign 1336 "self_or_null" (Num 0)
  , H.mkSeq 1336 1337
  , H.mkVar 1337 "method"
  , H.mkSeq 1337 1338
  , H.mkVar 1338 "temp"
  , H.mkSeq 1338 1339
  , H.mkAssign 1339 "callable" (Num 0)
  , H.mkSeq 1339 1340
  , H.mkAssign 1340 "undefed" (Num 0)
  , H.mkSeq 1340 1341
  , H.mkAssign 1341 "undefed" (Num 0)
  , H.mkSeq 1341 1342
  , H.mkAssign 1342 "stack_pointer" (Num 0)
  , H.mkSeq 1342 1343
  , H.mkSeq 1342 1344
  , H.mkVar 1343 "NOP_1343"
  , H.mkSeq 1343 1344
  , H.mkVar 1344 "IF_ELSE_FOOTER"
  , H.mkAssign 1345 "args" (Num 0)
  , H.mkSeq 1345 1346
  , H.mkVar 1346 "callable_o"
  , H.mkSeq 1346 1347
  , H.mkVar 1347 "total_args"
  , H.mkSeq 1347 1348
  , H.mkVar 1348 "arguments"
  , H.mkSeq 1348 1349
  , H.mkBranch 1349 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1351 1352
  , H.mkAssign 1351 "total_args" (Num 0)
  , H.mkSeq 1351 1352
  , H.mkSeq 1351 1353
  , H.mkVar 1352 "NOP_1352"
  , H.mkSeq 1352 1353
  , H.mkVar 1353 "IF_ELSE_FOOTER"
  , H.mkBranch 1354 (Eq (Plus (Plus (Plus (Num 0) (Plus (Id "PyFunction_Type") (Num 0))) (Plus (Num 0) (Num 0))) (Plus (Num 0) (Id "_PyFunction_Vectorcall"))) (Num 1)) 1356 1372
  , H.mkVar 1356 "code_flags"
  , H.mkSeq 1356 1357
  , H.mkVar 1357 "locals"
  , H.mkSeq 1357 1358
  , H.mkAssign 1358 "undefed" (Num 0)
  , H.mkSeq 1358 1359
  , H.mkAssign 1359 "undefed" (Num 0)
  , H.mkSeq 1359 1360
  , H.mkVar 1360 "new_frame"
  , H.mkSeq 1360 1361
  , H.mkAssign 1361 "stack_pointer" (Num 0)
  , H.mkSeq 1361 1362
  , H.mkAssign 1362 "stack_pointer" (Num 0)
  , H.mkSeq 1362 1363
  , H.mkBranch 1363 (Eq (Plus (Id "new_frame") (Num 0)) (Num 1)) 1365 1365
  , H.mkSeq 1364 1817
  , H.mkSeq 1364 1366
  , H.mkVar 1365 "NOP_1365"
  , H.mkSeq 1365 1366
  , H.mkVar 1366 "IF_ELSE_FOOTER"
  , H.mkAssign 1367 "undefed" (Num 0)
  , H.mkSeq 1367 1368
  , H.mkAssign 1368 "frame" (Num 0)
  , H.mkSeq 1368 1369
  , H.mkSeq 1368 1886
  , H.mkBranch 1369 (Eq (Num 0) (Num 1)) 1370 1371
  , H.mkAssign 1370 "frame" (Num 0)
  , H.mkSeq 1370 1371
  , H.mkSeq 1370 1886
  , H.mkSeq 1370 1369
  , H.mkVar 1371 "LOOP_FOOTER"
  , H.mkSeq 1371 1372
  , H.mkSeq 1371 1373
  , H.mkVar 1372 "NOP_1372"
  , H.mkSeq 1372 1373
  , H.mkVar 1373 "IF_ELSE_FOOTER"
  , H.mkVar 1374 "args_o_temp"
  , H.mkSeq 1374 1375
  , H.mkVar 1375 "args_o"
  , H.mkSeq 1375 1376
  , H.mkBranch 1376 (Eq (Plus (Id "args_o") (Num 0)) (Num 1)) 1378 1394
  , H.mkVar 1378 "tmp"
  , H.mkSeq 1378 1379
  , H.mkVar 1379 "_i"
  , H.mkSeq 1379 1380
  , H.mkBranch 1380 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1381 1385
  , H.mkAssign 1381 "tmp" (Num 0)
  , H.mkSeq 1381 1382
  , H.mkAssign 1382 "undefed" (Num 0)
  , H.mkSeq 1382 1383
  , H.mkAssign 1383 "undefed" (Num 0)
  , H.mkSeq 1383 1384
  , H.mkAssign 1384 "undefed" (Num 0)
  , H.mkSeq 1384 1385
  , H.mkSeq 1384 1380
  , H.mkVar 1385 "LOOP_FOOTER"
  , H.mkSeq 1385 1386
  , H.mkAssign 1386 "tmp" (Num 0)
  , H.mkSeq 1386 1387
  , H.mkAssign 1387 "self_or_null" (Num 0)
  , H.mkSeq 1387 1388
  , H.mkAssign 1388 "undefed" (Num 0)
  , H.mkSeq 1388 1389
  , H.mkAssign 1389 "tmp" (Num 0)
  , H.mkSeq 1389 1390
  , H.mkAssign 1390 "callable" (Num 0)
  , H.mkSeq 1390 1391
  , H.mkAssign 1391 "undefed" (Num 0)
  , H.mkSeq 1391 1392
  , H.mkAssign 1392 "stack_pointer" (Num 0)
  , H.mkSeq 1392 1393
  , H.mkAssign 1393 "stack_pointer" (Num 0)
  , H.mkSeq 1393 1394
  , H.mkSeq 1393 1817
  , H.mkSeq 1393 1395
  , H.mkVar 1394 "NOP_1394"
  , H.mkSeq 1394 1395
  , H.mkVar 1395 "IF_ELSE_FOOTER"
  , H.mkAssign 1396 "undefed" (Num 0)
  , H.mkSeq 1396 1397
  , H.mkAssign 1397 "undefed" (Num 0)
  , H.mkSeq 1397 1398
  , H.mkVar 1398 "res_o"
  , H.mkSeq 1398 1399
  , H.mkAssign 1399 "stack_pointer" (Num 0)
  , H.mkSeq 1399 1400
  , H.mkBranch 1400 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1402 1514
  , H.mkVar 1402 "arg"
  , H.mkSeq 1402 1403
  , H.mkBranch 1403 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 1405 1406
  , H.mkAssign 1405 "stack_pointer" (Num 0)
  , H.mkSeq 1405 1513
  , H.mkVar 1406 "err"
  , H.mkSeq 1406 1407
  , H.mkAssign 1407 "stack_pointer" (Num 0)
  , H.mkSeq 1407 1408
  , H.mkBranch 1408 (Eq (Plus (Id "err") (Num 0)) (Num 1)) 1410 1511
  , H.mkAssign 1410 "undefed" (Num 0)
  , H.mkSeq 1410 1411
  , H.mkAssign 1411 "_tmp_old_op" (Num 0)
  , H.mkSeq 1411 1412
  , H.mkBranch 1412 (Eq (Plus (Id "_tmp_old_op") (Num 0)) (Num 1)) 1414 1457
  , H.mkAssign 1414 "undefed" (Num 0)
  , H.mkSeq 1414 1415
  , H.mkVar 1415 "op"
  , H.mkSeq 1415 1416
  , H.mkBranch 1416 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1418 1433
  , H.mkVar 1418 "tracer"
  , H.mkSeq 1418 1419
  , H.mkBranch 1419 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1421 1422
  , H.mkVar 1421 "data"
  , H.mkSeq 1421 1422
  , H.mkSeq 1421 1423
  , H.mkVar 1422 "NOP_1422"
  , H.mkSeq 1422 1423
  , H.mkVar 1423 "IF_ELSE_FOOTER"
  , H.mkBranch 1424 (Eq (Num 0) (Num 1)) 1425 1431
  , H.mkVar 1425 "tracer"
  , H.mkSeq 1425 1426
  , H.mkBranch 1426 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1428 1429
  , H.mkVar 1428 "data"
  , H.mkSeq 1428 1429
  , H.mkSeq 1428 1430
  , H.mkVar 1429 "NOP_1429"
  , H.mkSeq 1429 1430
  , H.mkVar 1430 "IF_ELSE_FOOTER"
  , H.mkSeq 1430 1424
  , H.mkVar 1431 "LOOP_FOOTER"
  , H.mkSeq 1431 1432
  , H.mkVar 1432 "dealloc"
  , H.mkSeq 1432 1433
  , H.mkSeq 1432 1434
  , H.mkVar 1433 "NOP_1433"
  , H.mkSeq 1433 1434
  , H.mkVar 1434 "IF_ELSE_FOOTER"
  , H.mkBranch 1435 (Eq (Num 0) (Num 1)) 1436 1456
  , H.mkVar 1436 "op"
  , H.mkSeq 1436 1437
  , H.mkBranch 1437 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1439 1454
  , H.mkVar 1439 "tracer"
  , H.mkSeq 1439 1440
  , H.mkBranch 1440 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1442 1443
  , H.mkVar 1442 "data"
  , H.mkSeq 1442 1443
  , H.mkSeq 1442 1444
  , H.mkVar 1443 "NOP_1443"
  , H.mkSeq 1443 1444
  , H.mkVar 1444 "IF_ELSE_FOOTER"
  , H.mkBranch 1445 (Eq (Num 0) (Num 1)) 1446 1452
  , H.mkVar 1446 "tracer"
  , H.mkSeq 1446 1447
  , H.mkBranch 1447 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1449 1450
  , H.mkVar 1449 "data"
  , H.mkSeq 1449 1450
  , H.mkSeq 1449 1451
  , H.mkVar 1450 "NOP_1450"
  , H.mkSeq 1450 1451
  , H.mkVar 1451 "IF_ELSE_FOOTER"
  , H.mkSeq 1451 1445
  , H.mkVar 1452 "LOOP_FOOTER"
  , H.mkSeq 1452 1453
  , H.mkVar 1453 "dealloc"
  , H.mkSeq 1453 1454
  , H.mkSeq 1453 1455
  , H.mkVar 1454 "NOP_1454"
  , H.mkSeq 1454 1455
  , H.mkVar 1455 "IF_ELSE_FOOTER"
  , H.mkSeq 1455 1435
  , H.mkVar 1456 "LOOP_FOOTER"
  , H.mkSeq 1456 1457
  , H.mkSeq 1456 1458
  , H.mkVar 1457 "NOP_1457"
  , H.mkSeq 1457 1458
  , H.mkVar 1458 "IF_ELSE_FOOTER"
  , H.mkBranch 1459 (Eq (Num 0) (Num 1)) 1460 1509
  , H.mkAssign 1460 "undefed" (Num 0)
  , H.mkSeq 1460 1461
  , H.mkAssign 1461 "_tmp_old_op" (Num 0)
  , H.mkSeq 1461 1462
  , H.mkBranch 1462 (Eq (Plus (Id "_tmp_old_op") (Num 0)) (Num 1)) 1464 1507
  , H.mkAssign 1464 "undefed" (Num 0)
  , H.mkSeq 1464 1465
  , H.mkVar 1465 "op"
  , H.mkSeq 1465 1466
  , H.mkBranch 1466 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1468 1483
  , H.mkVar 1468 "tracer"
  , H.mkSeq 1468 1469
  , H.mkBranch 1469 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1471 1472
  , H.mkVar 1471 "data"
  , H.mkSeq 1471 1472
  , H.mkSeq 1471 1473
  , H.mkVar 1472 "NOP_1472"
  , H.mkSeq 1472 1473
  , H.mkVar 1473 "IF_ELSE_FOOTER"
  , H.mkBranch 1474 (Eq (Num 0) (Num 1)) 1475 1481
  , H.mkVar 1475 "tracer"
  , H.mkSeq 1475 1476
  , H.mkBranch 1476 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1478 1479
  , H.mkVar 1478 "data"
  , H.mkSeq 1478 1479
  , H.mkSeq 1478 1480
  , H.mkVar 1479 "NOP_1479"
  , H.mkSeq 1479 1480
  , H.mkVar 1480 "IF_ELSE_FOOTER"
  , H.mkSeq 1480 1474
  , H.mkVar 1481 "LOOP_FOOTER"
  , H.mkSeq 1481 1482
  , H.mkVar 1482 "dealloc"
  , H.mkSeq 1482 1483
  , H.mkSeq 1482 1484
  , H.mkVar 1483 "NOP_1483"
  , H.mkSeq 1483 1484
  , H.mkVar 1484 "IF_ELSE_FOOTER"
  , H.mkBranch 1485 (Eq (Num 0) (Num 1)) 1486 1506
  , H.mkVar 1486 "op"
  , H.mkSeq 1486 1487
  , H.mkBranch 1487 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1489 1504
  , H.mkVar 1489 "tracer"
  , H.mkSeq 1489 1490
  , H.mkBranch 1490 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1492 1493
  , H.mkVar 1492 "data"
  , H.mkSeq 1492 1493
  , H.mkSeq 1492 1494
  , H.mkVar 1493 "NOP_1493"
  , H.mkSeq 1493 1494
  , H.mkVar 1494 "IF_ELSE_FOOTER"
  , H.mkBranch 1495 (Eq (Num 0) (Num 1)) 1496 1502
  , H.mkVar 1496 "tracer"
  , H.mkSeq 1496 1497
  , H.mkBranch 1497 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1499 1500
  , H.mkVar 1499 "data"
  , H.mkSeq 1499 1500
  , H.mkSeq 1499 1501
  , H.mkVar 1500 "NOP_1500"
  , H.mkSeq 1500 1501
  , H.mkVar 1501 "IF_ELSE_FOOTER"
  , H.mkSeq 1501 1495
  , H.mkVar 1502 "LOOP_FOOTER"
  , H.mkSeq 1502 1503
  , H.mkVar 1503 "dealloc"
  , H.mkSeq 1503 1504
  , H.mkSeq 1503 1505
  , H.mkVar 1504 "NOP_1504"
  , H.mkSeq 1504 1505
  , H.mkVar 1505 "IF_ELSE_FOOTER"
  , H.mkSeq 1505 1485
  , H.mkVar 1506 "LOOP_FOOTER"
  , H.mkSeq 1506 1507
  , H.mkSeq 1506 1508
  , H.mkVar 1507 "NOP_1507"
  , H.mkSeq 1507 1508
  , H.mkVar 1508 "IF_ELSE_FOOTER"
  , H.mkSeq 1508 1459
  , H.mkVar 1509 "LOOP_FOOTER"
  , H.mkSeq 1509 1510
  , H.mkAssign 1510 "stack_pointer" (Num 0)
  , H.mkSeq 1510 1511
  , H.mkSeq 1510 1512
  , H.mkVar 1511 "NOP_1511"
  , H.mkSeq 1511 1512
  , H.mkVar 1512 "IF_ELSE_FOOTER"
  , H.mkVar 1513 "IF_ELSE_FOOTER"
  , H.mkSeq 1513 1515
  , H.mkVar 1514 "NOP_1514"
  , H.mkSeq 1514 1515
  , H.mkVar 1515 "IF_ELSE_FOOTER"
  , H.mkVar 1516 "tmp"
  , H.mkSeq 1516 1517
  , H.mkVar 1517 "_i"
  , H.mkSeq 1517 1518
  , H.mkBranch 1518 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1519 1521
  , H.mkAssign 1519 "tmp" (Num 0)
  , H.mkSeq 1519 1520
  , H.mkAssign 1520 "undefed" (Num 0)
  , H.mkSeq 1520 1521
  , H.mkSeq 1520 1518
  , H.mkVar 1521 "LOOP_FOOTER"
  , H.mkSeq 1521 1522
  , H.mkAssign 1522 "tmp" (Num 0)
  , H.mkSeq 1522 1523
  , H.mkAssign 1523 "self_or_null" (Num 0)
  , H.mkSeq 1523 1524
  , H.mkAssign 1524 "undefed" (Num 0)
  , H.mkSeq 1524 1525
  , H.mkAssign 1525 "tmp" (Num 0)
  , H.mkSeq 1525 1526
  , H.mkAssign 1526 "callable" (Num 0)
  , H.mkSeq 1526 1527
  , H.mkAssign 1527 "undefed" (Num 0)
  , H.mkSeq 1527 1528
  , H.mkAssign 1528 "stack_pointer" (Num 0)
  , H.mkSeq 1528 1529
  , H.mkAssign 1529 "stack_pointer" (Num 0)
  , H.mkSeq 1529 1530
  , H.mkBranch 1530 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 1532 1532
  , H.mkSeq 1531 1817
  , H.mkSeq 1531 1533
  , H.mkVar 1532 "NOP_1532"
  , H.mkSeq 1532 1533
  , H.mkVar 1533 "IF_ELSE_FOOTER"
  , H.mkAssign 1534 "res" (Num 0)
  , H.mkSeq 1534 1535
  , H.mkAssign 1535 "undefed" (Num 0)
  , H.mkSeq 1535 1536
  , H.mkAssign 1536 "stack_pointer" (Num 0)
  , H.mkSeq 1536 1537
  , H.mkVar 1537 "err"
  , H.mkSeq 1537 1538
  , H.mkAssign 1538 "stack_pointer" (Num 0)
  , H.mkSeq 1538 1539
  , H.mkBranch 1539 (Eq (Plus (Id "err") (Num 0)) (Num 1)) 1541 1541
  , H.mkSeq 1540 1817
  , H.mkSeq 1540 1542
  , H.mkVar 1541 "NOP_1541"
  , H.mkSeq 1541 1542
  , H.mkVar 1542 "IF_ELSE_FOOTER"
  , H.mkVar 1543 "word"
  , H.mkSeq 1543 1544
  , H.mkAssign 1544 "opcode" (Num 0)
  , H.mkSeq 1544 1545
  , H.mkAssign 1545 "oparg" (Num 0)
  , H.mkSeq 1545 1546
  , H.mkBranch 1546 (Eq (Num 0) (Num 1)) 1547 1550
  , H.mkVar 1547 "word"
  , H.mkSeq 1547 1548
  , H.mkAssign 1548 "opcode" (Num 0)
  , H.mkSeq 1548 1549
  , H.mkAssign 1549 "oparg" (Num 0)
  , H.mkSeq 1549 1550
  , H.mkSeq 1549 1546
  , H.mkVar 1550 "LOOP_FOOTER"
  , H.mkSeq 1550 1551
  , H.mkSeq 1550 35
  , H.mkBranch 1551 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1553 1637
  , H.mkVar 1553 "NOP_1553"
  , H.mkVar 1554 "__CLABEL_TARGET_CALL_ALLOC_AND_ENTER_INIT"
  , H.mkSeq 1554 1555
  , H.mkVar 1555 "this_instr"
  , H.mkSeq 1555 1556
  , H.mkAssign 1556 "undefed" (Num 0)
  , H.mkSeq 1556 1557
  , H.mkAssign 1557 "next_instr" (Num 0)
  , H.mkSeq 1557 1558
  , H.mkVar 1558 "callable"
  , H.mkSeq 1558 1559
  , H.mkVar 1559 "self_or_null"
  , H.mkSeq 1559 1560
  , H.mkVar 1560 "init"
  , H.mkSeq 1560 1561
  , H.mkVar 1561 "self"
  , H.mkSeq 1561 1562
  , H.mkVar 1562 "args"
  , H.mkSeq 1562 1563
  , H.mkVar 1563 "init_frame"
  , H.mkSeq 1563 1564
  , H.mkVar 1564 "new_frame"
  , H.mkSeq 1564 1565
  , H.mkBranch 1565 (Eq (Num 0) (Num 1)) 1567 1567
  , H.mkSeq 1566 1310
  , H.mkSeq 1566 1568
  , H.mkVar 1567 "NOP_1567"
  , H.mkSeq 1567 1568
  , H.mkVar 1568 "IF_ELSE_FOOTER"
  , H.mkAssign 1569 "self_or_null" (Num 0)
  , H.mkSeq 1569 1570
  , H.mkAssign 1570 "callable" (Num 0)
  , H.mkSeq 1570 1571
  , H.mkVar 1571 "type_version"
  , H.mkSeq 1571 1572
  , H.mkVar 1572 "callable_o"
  , H.mkSeq 1572 1573
  , H.mkBranch 1573 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1575 1575
  , H.mkSeq 1574 1310
  , H.mkSeq 1574 1576
  , H.mkVar 1575 "NOP_1575"
  , H.mkSeq 1575 1576
  , H.mkVar 1576 "IF_ELSE_FOOTER"
  , H.mkBranch 1577 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1579 1579
  , H.mkSeq 1578 1310
  , H.mkSeq 1578 1580
  , H.mkVar 1579 "NOP_1579"
  , H.mkSeq 1579 1580
  , H.mkVar 1580 "IF_ELSE_FOOTER"
  , H.mkVar 1581 "tp"
  , H.mkSeq 1581 1582
  , H.mkBranch 1582 (Eq (Plus (Num 0) (Id "type_version")) (Num 1)) 1584 1584
  , H.mkSeq 1583 1310
  , H.mkSeq 1583 1585
  , H.mkVar 1584 "NOP_1584"
  , H.mkSeq 1584 1585
  , H.mkVar 1585 "IF_ELSE_FOOTER"
  , H.mkVar 1586 "cls"
  , H.mkSeq 1586 1587
  , H.mkVar 1587 "init_func"
  , H.mkSeq 1587 1588
  , H.mkVar 1588 "code"
  , H.mkSeq 1588 1589
  , H.mkBranch 1589 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1591 1591
  , H.mkSeq 1590 1310
  , H.mkSeq 1590 1592
  , H.mkVar 1591 "NOP_1591"
  , H.mkSeq 1591 1592
  , H.mkVar 1592 "IF_ELSE_FOOTER"
  , H.mkVar 1593 "self_o"
  , H.mkSeq 1593 1594
  , H.mkAssign 1594 "stack_pointer" (Num 0)
  , H.mkSeq 1594 1595
  , H.mkBranch 1595 (Eq (Plus (Id "self_o") (Num 0)) (Num 1)) 1597 1597
  , H.mkSeq 1596 1817
  , H.mkSeq 1596 1598
  , H.mkVar 1597 "NOP_1597"
  , H.mkSeq 1597 1598
  , H.mkVar 1598 "IF_ELSE_FOOTER"
  , H.mkAssign 1599 "self_or_null" (Num 0)
  , H.mkSeq 1599 1600
  , H.mkVar 1600 "temp"
  , H.mkSeq 1600 1601
  , H.mkAssign 1601 "callable" (Num 0)
  , H.mkSeq 1601 1602
  , H.mkAssign 1602 "undefed" (Num 0)
  , H.mkSeq 1602 1603
  , H.mkAssign 1603 "undefed" (Num 0)
  , H.mkSeq 1603 1604
  , H.mkAssign 1604 "stack_pointer" (Num 0)
  , H.mkSeq 1604 1605
  , H.mkAssign 1605 "args" (Num 0)
  , H.mkSeq 1605 1606
  , H.mkAssign 1606 "self" (Num 0)
  , H.mkSeq 1606 1607
  , H.mkAssign 1607 "init" (Num 0)
  , H.mkSeq 1607 1608
  , H.mkVar 1608 "shim"
  , H.mkSeq 1608 1609
  , H.mkAssign 1609 "stack_pointer" (Num 0)
  , H.mkSeq 1609 1610
  , H.mkAssign 1610 "undefed" (Num 0)
  , H.mkSeq 1610 1611
  , H.mkVar 1611 "temp"
  , H.mkSeq 1611 1612
  , H.mkAssign 1612 "stack_pointer" (Num 0)
  , H.mkSeq 1612 1613
  , H.mkAssign 1613 "stack_pointer" (Num 0)
  , H.mkSeq 1613 1614
  , H.mkBranch 1614 (Eq (Plus (Id "temp") (Num 0)) (Num 1)) 1616 1617
  , H.mkAssign 1616 "stack_pointer" (Num 0)
  , H.mkSeq 1616 1617
  , H.mkSeq 1616 1817
  , H.mkSeq 1616 1618
  , H.mkVar 1617 "NOP_1617"
  , H.mkSeq 1617 1618
  , H.mkVar 1618 "IF_ELSE_FOOTER"
  , H.mkAssign 1619 "undefed" (Num 0)
  , H.mkSeq 1619 1620
  , H.mkAssign 1620 "init_frame" (Num 0)
  , H.mkSeq 1620 1621
  , H.mkAssign 1621 "new_frame" (Num 0)
  , H.mkSeq 1621 1622
  , H.mkVar 1622 "temp"
  , H.mkSeq 1622 1623
  , H.mkAssign 1623 "frame" (Num 0)
  , H.mkSeq 1623 1624
  , H.mkAssign 1624 "stack_pointer" (Num 0)
  , H.mkSeq 1624 1625
  , H.mkAssign 1625 "next_instr" (Num 0)
  , H.mkSeq 1625 1626
  , H.mkBranch 1626 (Eq (Num 0) (Num 1)) 1627 1628
  , H.mkAssign 1627 "next_instr" (Num 0)
  , H.mkSeq 1627 1626
  , H.mkVar 1628 "LOOP_FOOTER"
  , H.mkSeq 1628 1629
  , H.mkVar 1629 "word"
  , H.mkSeq 1629 1630
  , H.mkAssign 1630 "opcode" (Num 0)
  , H.mkSeq 1630 1631
  , H.mkAssign 1631 "oparg" (Num 0)
  , H.mkSeq 1631 1632
  , H.mkBranch 1632 (Eq (Num 0) (Num 1)) 1633 1636
  , H.mkVar 1633 "word"
  , H.mkSeq 1633 1634
  , H.mkAssign 1634 "opcode" (Num 0)
  , H.mkSeq 1634 1635
  , H.mkAssign 1635 "oparg" (Num 0)
  , H.mkSeq 1635 1636
  , H.mkSeq 1635 1632
  , H.mkVar 1636 "LOOP_FOOTER"
  , H.mkSeq 1636 1637
  , H.mkSeq 1636 35
  , H.mkBranch 1637 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1639 1727
  , H.mkVar 1639 "NOP_1639"
  , H.mkVar 1640 "__CLABEL_TARGET_CALL_BOUND_METHOD_EXACT_ARGS"
  , H.mkSeq 1640 1641
  , H.mkVar 1641 "this_instr"
  , H.mkSeq 1641 1642
  , H.mkAssign 1642 "undefed" (Num 0)
  , H.mkSeq 1642 1643
  , H.mkAssign 1643 "next_instr" (Num 0)
  , H.mkSeq 1643 1644
  , H.mkVar 1644 "callable"
  , H.mkSeq 1644 1645
  , H.mkVar 1645 "null"
  , H.mkSeq 1645 1646
  , H.mkVar 1646 "self_or_null"
  , H.mkSeq 1646 1647
  , H.mkVar 1647 "args"
  , H.mkSeq 1647 1648
  , H.mkVar 1648 "new_frame"
  , H.mkSeq 1648 1649
  , H.mkBranch 1649 (Eq (Num 0) (Num 1)) 1651 1651
  , H.mkSeq 1650 1310
  , H.mkSeq 1650 1652
  , H.mkVar 1651 "NOP_1651"
  , H.mkSeq 1651 1652
  , H.mkVar 1652 "IF_ELSE_FOOTER"
  , H.mkAssign 1653 "null" (Num 0)
  , H.mkSeq 1653 1654
  , H.mkAssign 1654 "callable" (Num 0)
  , H.mkSeq 1654 1655
  , H.mkBranch 1655 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1657 1657
  , H.mkSeq 1656 1310
  , H.mkSeq 1656 1658
  , H.mkVar 1657 "NOP_1657"
  , H.mkSeq 1657 1658
  , H.mkVar 1658 "IF_ELSE_FOOTER"
  , H.mkBranch 1659 (Eq (Plus (Num 0) (Plus (Id "PyMethod_Type") (Num 0))) (Num 1)) 1661 1661
  , H.mkSeq 1660 1310
  , H.mkSeq 1660 1662
  , H.mkVar 1661 "NOP_1661"
  , H.mkSeq 1661 1662
  , H.mkVar 1662 "IF_ELSE_FOOTER"
  , H.mkAssign 1663 "self_or_null" (Num 0)
  , H.mkSeq 1663 1664
  , H.mkVar 1664 "callable_o"
  , H.mkSeq 1664 1665
  , H.mkAssign 1665 "self_or_null" (Num 0)
  , H.mkSeq 1665 1666
  , H.mkVar 1666 "temp"
  , H.mkSeq 1666 1667
  , H.mkAssign 1667 "callable" (Num 0)
  , H.mkSeq 1667 1668
  , H.mkAssign 1668 "undefed" (Num 0)
  , H.mkSeq 1668 1669
  , H.mkAssign 1669 "undefed" (Num 0)
  , H.mkSeq 1669 1670
  , H.mkAssign 1670 "stack_pointer" (Num 0)
  , H.mkSeq 1670 1671
  , H.mkVar 1671 "func_version"
  , H.mkSeq 1671 1672
  , H.mkVar 1672 "callable_o"
  , H.mkSeq 1672 1673
  , H.mkBranch 1673 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFunction_Type") (Num 0))) (Num 0)) (Num 1)) 1675 1675
  , H.mkSeq 1674 1310
  , H.mkSeq 1674 1676
  , H.mkVar 1675 "NOP_1675"
  , H.mkSeq 1675 1676
  , H.mkVar 1676 "IF_ELSE_FOOTER"
  , H.mkVar 1677 "func"
  , H.mkSeq 1677 1678
  , H.mkBranch 1678 (Eq (Plus (Num 0) (Id "func_version")) (Num 1)) 1680 1680
  , H.mkSeq 1679 1310
  , H.mkSeq 1679 1681
  , H.mkVar 1680 "NOP_1680"
  , H.mkSeq 1680 1681
  , H.mkVar 1681 "IF_ELSE_FOOTER"
  , H.mkVar 1682 "callable_o"
  , H.mkSeq 1682 1683
  , H.mkVar 1683 "func"
  , H.mkSeq 1683 1684
  , H.mkVar 1684 "code"
  , H.mkSeq 1684 1685
  , H.mkBranch 1685 (Eq (Plus (Num 0) (Plus (Id "oparg") (Plus (Plus (Num 0) (Num 0)) (Num 0)))) (Num 1)) 1687 1687
  , H.mkSeq 1686 1310
  , H.mkSeq 1686 1688
  , H.mkVar 1687 "NOP_1687"
  , H.mkSeq 1687 1688
  , H.mkVar 1688 "IF_ELSE_FOOTER"
  , H.mkVar 1689 "callable_o"
  , H.mkSeq 1689 1690
  , H.mkVar 1690 "func"
  , H.mkSeq 1690 1691
  , H.mkVar 1691 "code"
  , H.mkSeq 1691 1692
  , H.mkBranch 1692 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1694 1694
  , H.mkSeq 1693 1310
  , H.mkSeq 1693 1695
  , H.mkVar 1694 "NOP_1694"
  , H.mkSeq 1694 1695
  , H.mkVar 1695 "IF_ELSE_FOOTER"
  , H.mkBranch 1696 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1698 1698
  , H.mkSeq 1697 1310
  , H.mkSeq 1697 1699
  , H.mkVar 1698 "NOP_1698"
  , H.mkSeq 1698 1699
  , H.mkVar 1699 "IF_ELSE_FOOTER"
  , H.mkAssign 1700 "args" (Num 0)
  , H.mkSeq 1700 1701
  , H.mkVar 1701 "has_self"
  , H.mkSeq 1701 1702
  , H.mkVar 1702 "pushed_frame"
  , H.mkSeq 1702 1703
  , H.mkVar 1703 "first_non_self_local"
  , H.mkSeq 1703 1704
  , H.mkAssign 1704 "undefed" (Num 0)
  , H.mkSeq 1704 1705
  , H.mkVar 1705 "i"
  , H.mkSeq 1705 1706
  , H.mkBranch 1706 (Eq (Plus (Id "i") (Id "oparg")) (Num 1)) 1707 1708
  , H.mkAssign 1707 "undefed" (Num 0)
  , H.mkSeq 1707 1708
  , H.mkSeq 1707 1706
  , H.mkVar 1708 "LOOP_FOOTER"
  , H.mkSeq 1708 1709
  , H.mkAssign 1709 "new_frame" (Num 0)
  , H.mkSeq 1709 1710
  , H.mkAssign 1710 "undefed" (Num 0)
  , H.mkSeq 1710 1711
  , H.mkVar 1711 "temp"
  , H.mkSeq 1711 1712
  , H.mkAssign 1712 "stack_pointer" (Num 0)
  , H.mkSeq 1712 1713
  , H.mkAssign 1713 "frame" (Num 0)
  , H.mkSeq 1713 1714
  , H.mkAssign 1714 "stack_pointer" (Num 0)
  , H.mkSeq 1714 1715
  , H.mkAssign 1715 "next_instr" (Num 0)
  , H.mkSeq 1715 1716
  , H.mkBranch 1716 (Eq (Num 0) (Num 1)) 1717 1718
  , H.mkAssign 1717 "next_instr" (Num 0)
  , H.mkSeq 1717 1716
  , H.mkVar 1718 "LOOP_FOOTER"
  , H.mkSeq 1718 1719
  , H.mkVar 1719 "word"
  , H.mkSeq 1719 1720
  , H.mkAssign 1720 "opcode" (Num 0)
  , H.mkSeq 1720 1721
  , H.mkAssign 1721 "oparg" (Num 0)
  , H.mkSeq 1721 1722
  , H.mkBranch 1722 (Eq (Num 0) (Num 1)) 1723 1726
  , H.mkVar 1723 "word"
  , H.mkSeq 1723 1724
  , H.mkAssign 1724 "opcode" (Num 0)
  , H.mkSeq 1724 1725
  , H.mkAssign 1725 "oparg" (Num 0)
  , H.mkSeq 1725 1726
  , H.mkSeq 1725 1722
  , H.mkVar 1726 "LOOP_FOOTER"
  , H.mkSeq 1726 1727
  , H.mkSeq 1726 35
  , H.mkBranch 1727 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1729 1810
  , H.mkVar 1729 "NOP_1729"
  , H.mkVar 1730 "__CLABEL_TARGET_CALL_BOUND_METHOD_GENERAL"
  , H.mkSeq 1730 1731
  , H.mkVar 1731 "this_instr"
  , H.mkSeq 1731 1732
  , H.mkAssign 1732 "undefed" (Num 0)
  , H.mkSeq 1732 1733
  , H.mkAssign 1733 "next_instr" (Num 0)
  , H.mkSeq 1733 1734
  , H.mkVar 1734 "callable"
  , H.mkSeq 1734 1735
  , H.mkVar 1735 "null"
  , H.mkSeq 1735 1736
  , H.mkVar 1736 "self_or_null"
  , H.mkSeq 1736 1737
  , H.mkVar 1737 "args"
  , H.mkSeq 1737 1738
  , H.mkVar 1738 "new_frame"
  , H.mkSeq 1738 1739
  , H.mkBranch 1739 (Eq (Num 0) (Num 1)) 1741 1741
  , H.mkSeq 1740 1310
  , H.mkSeq 1740 1742
  , H.mkVar 1741 "NOP_1741"
  , H.mkSeq 1741 1742
  , H.mkVar 1742 "IF_ELSE_FOOTER"
  , H.mkAssign 1743 "null" (Num 0)
  , H.mkSeq 1743 1744
  , H.mkAssign 1744 "callable" (Num 0)
  , H.mkSeq 1744 1745
  , H.mkVar 1745 "func_version"
  , H.mkSeq 1745 1746
  , H.mkVar 1746 "callable_o"
  , H.mkSeq 1746 1747
  , H.mkBranch 1747 (Eq (Plus (Num 0) (Plus (Id "PyMethod_Type") (Num 0))) (Num 1)) 1749 1749
  , H.mkSeq 1748 1310
  , H.mkSeq 1748 1750
  , H.mkVar 1749 "NOP_1749"
  , H.mkSeq 1749 1750
  , H.mkVar 1750 "IF_ELSE_FOOTER"
  , H.mkVar 1751 "func"
  , H.mkSeq 1751 1752
  , H.mkBranch 1752 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFunction_Type") (Num 0))) (Num 0)) (Num 1)) 1754 1754
  , H.mkSeq 1753 1310
  , H.mkSeq 1753 1755
  , H.mkVar 1754 "NOP_1754"
  , H.mkSeq 1754 1755
  , H.mkVar 1755 "IF_ELSE_FOOTER"
  , H.mkBranch 1756 (Eq (Plus (Num 0) (Id "func_version")) (Num 1)) 1758 1758
  , H.mkSeq 1757 1310
  , H.mkSeq 1757 1759
  , H.mkVar 1758 "NOP_1758"
  , H.mkSeq 1758 1759
  , H.mkVar 1759 "IF_ELSE_FOOTER"
  , H.mkBranch 1760 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1762 1762
  , H.mkSeq 1761 1310
  , H.mkSeq 1761 1763
  , H.mkVar 1762 "NOP_1762"
  , H.mkSeq 1762 1763
  , H.mkVar 1763 "IF_ELSE_FOOTER"
  , H.mkAssign 1764 "self_or_null" (Num 0)
  , H.mkSeq 1764 1765
  , H.mkVar 1765 "callable_o"
  , H.mkSeq 1765 1766
  , H.mkAssign 1766 "self_or_null" (Num 0)
  , H.mkSeq 1766 1767
  , H.mkVar 1767 "temp"
  , H.mkSeq 1767 1768
  , H.mkAssign 1768 "callable" (Num 0)
  , H.mkSeq 1768 1769
  , H.mkAssign 1769 "undefed" (Num 0)
  , H.mkSeq 1769 1770
  , H.mkAssign 1770 "undefed" (Num 0)
  , H.mkSeq 1770 1771
  , H.mkAssign 1771 "stack_pointer" (Num 0)
  , H.mkSeq 1771 1772
  , H.mkBranch 1772 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1774 1774
  , H.mkSeq 1773 1310
  , H.mkSeq 1773 1775
  , H.mkVar 1774 "NOP_1774"
  , H.mkSeq 1774 1775
  , H.mkVar 1775 "IF_ELSE_FOOTER"
  , H.mkAssign 1776 "args" (Num 0)
  , H.mkSeq 1776 1777
  , H.mkVar 1777 "callable_o"
  , H.mkSeq 1777 1778
  , H.mkVar 1778 "total_args"
  , H.mkSeq 1778 1779
  , H.mkBranch 1779 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1781 1782
  , H.mkAssign 1781 "total_args" (Num 0)
  , H.mkSeq 1781 1782
  , H.mkSeq 1781 1783
  , H.mkVar 1782 "NOP_1782"
  , H.mkSeq 1782 1783
  , H.mkVar 1783 "IF_ELSE_FOOTER"
  , H.mkVar 1784 "code_flags"
  , H.mkSeq 1784 1785
  , H.mkVar 1785 "locals"
  , H.mkSeq 1785 1786
  , H.mkVar 1786 "temp"
  , H.mkSeq 1786 1787
  , H.mkAssign 1787 "stack_pointer" (Num 0)
  , H.mkSeq 1787 1788
  , H.mkAssign 1788 "stack_pointer" (Num 0)
  , H.mkSeq 1788 1789
  , H.mkBranch 1789 (Eq (Plus (Id "temp") (Num 0)) (Num 1)) 1791 1791
  , H.mkSeq 1790 1817
  , H.mkSeq 1790 1792
  , H.mkVar 1791 "NOP_1791"
  , H.mkSeq 1791 1792
  , H.mkVar 1792 "IF_ELSE_FOOTER"
  , H.mkAssign 1793 "new_frame" (Num 0)
  , H.mkSeq 1793 1794
  , H.mkAssign 1794 "undefed" (Num 0)
  , H.mkSeq 1794 1795
  , H.mkVar 1795 "temp"
  , H.mkSeq 1795 1796
  , H.mkAssign 1796 "frame" (Num 0)
  , H.mkSeq 1796 1797
  , H.mkAssign 1797 "stack_pointer" (Num 0)
  , H.mkSeq 1797 1798
  , H.mkAssign 1798 "next_instr" (Num 0)
  , H.mkSeq 1798 1799
  , H.mkBranch 1799 (Eq (Num 0) (Num 1)) 1800 1801
  , H.mkAssign 1800 "next_instr" (Num 0)
  , H.mkSeq 1800 1799
  , H.mkVar 1801 "LOOP_FOOTER"
  , H.mkSeq 1801 1802
  , H.mkVar 1802 "word"
  , H.mkSeq 1802 1803
  , H.mkAssign 1803 "opcode" (Num 0)
  , H.mkSeq 1803 1804
  , H.mkAssign 1804 "oparg" (Num 0)
  , H.mkSeq 1804 1805
  , H.mkBranch 1805 (Eq (Num 0) (Num 1)) 1806 1809
  , H.mkVar 1806 "word"
  , H.mkSeq 1806 1807
  , H.mkAssign 1807 "opcode" (Num 0)
  , H.mkSeq 1807 1808
  , H.mkAssign 1808 "oparg" (Num 0)
  , H.mkSeq 1808 1809
  , H.mkSeq 1808 1805
  , H.mkVar 1809 "LOOP_FOOTER"
  , H.mkSeq 1809 1810
  , H.mkSeq 1809 35
  , H.mkVar 1810 "NOP_1810"
  , H.mkSeq 1810 1811
  , H.mkVar 1811 "__CLABEL_CODEGEN_SWITCH_EXIT_0"
  , H.mkSeq 1811 1812
  , H.mkVar 1812 "NOP_1812"
  , H.mkVar 1813 "__CLABEL_pop_2_error"
  , H.mkSeq 1813 1814
  , H.mkAssign 1814 "stack_pointer" (Num 0)
  , H.mkSeq 1814 1815
  , H.mkSeq 1814 1817
  , H.mkVar 1815 "__CLABEL_pop_1_error"
  , H.mkSeq 1815 1816
  , H.mkAssign 1816 "stack_pointer" (Num 0)
  , H.mkSeq 1816 1817
  , H.mkSeq 1816 1817
  , H.mkVar 1817 "__CLABEL_error"
  , H.mkSeq 1817 1818
  , H.mkBranch 1818 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1820 1821
  , H.mkAssign 1820 "stack_pointer" (Num 0)
  , H.mkSeq 1820 1822
  , H.mkVar 1821 "NOP_1821"
  , H.mkSeq 1821 1822
  , H.mkVar 1822 "IF_ELSE_FOOTER"
  , H.mkBranch 1823 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1825 1832
  , H.mkVar 1825 "f"
  , H.mkSeq 1825 1826
  , H.mkAssign 1826 "stack_pointer" (Num 0)
  , H.mkSeq 1826 1827
  , H.mkBranch 1827 (Eq (Plus (Id "f") (Num 0)) (Num 1)) 1829 1830
  , H.mkAssign 1829 "stack_pointer" (Num 0)
  , H.mkSeq 1829 1831
  , H.mkVar 1830 "NOP_1830"
  , H.mkSeq 1830 1831
  , H.mkVar 1831 "IF_ELSE_FOOTER"
  , H.mkSeq 1831 1833
  , H.mkVar 1832 "NOP_1832"
  , H.mkSeq 1832 1833
  , H.mkVar 1833 "IF_ELSE_FOOTER"
  , H.mkSeq 1833 1834
  , H.mkVar 1834 "__CLABEL_exception_unwind"
  , H.mkSeq 1834 1835
  , H.mkVar 1835 "offset"
  , H.mkSeq 1835 1836
  , H.mkVar 1836 "level"
  , H.mkSeq 1836 1837
  , H.mkVar 1837 "handler"
  , H.mkSeq 1837 1838
  , H.mkVar 1838 "lasti"
  , H.mkSeq 1838 1839
  , H.mkVar 1839 "handled"
  , H.mkSeq 1839 1840
  , H.mkBranch 1840 (Eq (Plus (Id "handled") (Num 0)) (Num 1)) 1842 1846
  , H.mkVar 1842 "stackbase"
  , H.mkSeq 1842 1843
  , H.mkBranch 1843 (Eq (Plus (Num 0) (Id "stackbase")) (Num 1)) 1844 1845
  , H.mkVar 1844 "ref"
  , H.mkSeq 1844 1845
  , H.mkSeq 1844 1843
  , H.mkVar 1845 "LOOP_FOOTER"
  , H.mkSeq 1845 1846
  , H.mkSeq 1845 1874
  , H.mkSeq 1845 1847
  , H.mkVar 1846 "NOP_1846"
  , H.mkSeq 1846 1847
  , H.mkVar 1847 "IF_ELSE_FOOTER"
  , H.mkVar 1848 "new_top"
  , H.mkSeq 1848 1849
  , H.mkBranch 1849 (Eq (Plus (Num 0) (Id "new_top")) (Num 1)) 1850 1851
  , H.mkVar 1850 "ref"
  , H.mkSeq 1850 1851
  , H.mkSeq 1850 1849
  , H.mkVar 1851 "LOOP_FOOTER"
  , H.mkSeq 1851 1852
  , H.mkBranch 1852 (Eq (Id "lasti") (Num 1)) 1854 1856
  , H.mkVar 1854 "frame_lasti"
  , H.mkSeq 1854 1855
  , H.mkVar 1855 "lasti"
  , H.mkSeq 1855 1856
  , H.mkSeq 1855 1857
  , H.mkVar 1856 "NOP_1856"
  , H.mkSeq 1856 1857
  , H.mkVar 1857 "IF_ELSE_FOOTER"
  , H.mkVar 1858 "exc"
  , H.mkSeq 1858 1859
  , H.mkAssign 1859 "next_instr" (Num 0)
  , H.mkSeq 1859 1860
  , H.mkVar 1860 "err"
  , H.mkSeq 1860 1861
  , H.mkBranch 1861 (Eq (Plus (Id "err") (Num 0)) (Num 1)) 1863 1863
  , H.mkSeq 1862 1834
  , H.mkSeq 1862 1864
  , H.mkVar 1863 "NOP_1863"
  , H.mkSeq 1863 1864
  , H.mkVar 1864 "IF_ELSE_FOOTER"
  , H.mkAssign 1865 "stack_pointer" (Num 0)
  , H.mkSeq 1865 1866
  , H.mkVar 1866 "word"
  , H.mkSeq 1866 1867
  , H.mkAssign 1867 "opcode" (Num 0)
  , H.mkSeq 1867 1868
  , H.mkAssign 1868 "oparg" (Num 0)
  , H.mkSeq 1868 1869
  , H.mkBranch 1869 (Eq (Num 0) (Num 1)) 1870 1873
  , H.mkVar 1870 "word"
  , H.mkSeq 1870 1871
  , H.mkAssign 1871 "opcode" (Num 0)
  , H.mkSeq 1871 1872
  , H.mkAssign 1872 "oparg" (Num 0)
  , H.mkSeq 1872 1873
  , H.mkSeq 1872 1869
  , H.mkVar 1873 "LOOP_FOOTER"
  , H.mkSeq 1873 1874
  , H.mkSeq 1873 35
  , H.mkVar 1874 "__CLABEL_exit_unwind"
  , H.mkSeq 1874 1875
  , H.mkVar 1875 "dying"
  , H.mkSeq 1875 1876
  , H.mkAssign 1876 "frame" (Num 0)
  , H.mkSeq 1876 1877
  , H.mkAssign 1877 "undefed" (Num 0)
  , H.mkSeq 1877 1878
  , H.mkBranch 1878 (Eq (Plus (Num 0) (Id "FRAME_OWNED_BY_INTERPRETER")) (Num 1)) 1880 1882
  , H.mkAssign 1880 "undefed" (Num 0)
  , H.mkSeq 1880 1881
  , H.mkAssign 1881 "return" (Num 0)
  , H.mkSeq 1881 1882
  , H.mkSeq 1881 1883
  , H.mkVar 1882 "NOP_1882"
  , H.mkSeq 1882 1883
  , H.mkVar 1883 "IF_ELSE_FOOTER"
  , H.mkAssign 1884 "next_instr" (Num 0)
  , H.mkSeq 1884 1885
  , H.mkAssign 1885 "stack_pointer" (Num 0)
  , H.mkSeq 1885 1886
  , H.mkSeq 1885 1817
  , H.mkVar 1886 "__CLABEL_start_frame"
  , H.mkSeq 1886 1887
  , H.mkVar 1887 "too_deep"
  , H.mkSeq 1887 1888
  , H.mkBranch 1888 (Eq (Id "too_deep") (Num 1)) 1890 1890
  , H.mkSeq 1889 1874
  , H.mkSeq 1889 1891
  , H.mkVar 1890 "NOP_1890"
  , H.mkSeq 1890 1891
  , H.mkVar 1891 "IF_ELSE_FOOTER"
  , H.mkAssign 1892 "next_instr" (Num 0)
  , H.mkSeq 1892 1893
  , H.mkAssign 1893 "stack_pointer" (Num 0)
  , H.mkSeq 1893 1894
  , H.mkVar 1894 "word"
  , H.mkSeq 1894 1895
  , H.mkAssign 1895 "opcode" (Num 0)
  , H.mkSeq 1895 1896
  , H.mkAssign 1896 "oparg" (Num 0)
  , H.mkSeq 1896 1897
  , H.mkBranch 1897 (Eq (Num 0) (Num 1)) 1898 1901
  , H.mkVar 1898 "word"
  , H.mkSeq 1898 1899
  , H.mkAssign 1899 "opcode" (Num 0)
  , H.mkSeq 1899 1900
  , H.mkAssign 1900 "oparg" (Num 0)
  , H.mkSeq 1900 1901
  , H.mkSeq 1900 1897
  , H.mkVar 1901 "LOOP_FOOTER"
  , H.mkSeq 1901 1902
  , H.mkSeq 1901 35
  , H.mkVar 1902 "__CLABEL_early_exit"
  , H.mkSeq 1902 1903
  , H.mkVar 1903 "NOP_1903"
  , H.mkVar 1904 "dying"
  , H.mkSeq 1904 1905
  , H.mkAssign 1905 "frame" (Num 0)
  , H.mkSeq 1905 1906
  , H.mkAssign 1906 "undefed" (Num 0)
  , H.mkSeq 1906 1907
  , H.mkAssign 1907 "undefed" (Num 0)
  , H.mkSeq 1907 1908
  , H.mkAssign 1908 "return" (Num 0)
  , H.mkSeq 1908 1909
  , H.mkSeq 1908 1909
  , H.mkVar 1909 "PROG_END"
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
  , NP.Branch 7 (Eq (Num 0) (Num 1)) 9 10
  , NP.Assign 9 "return" (Num 0)
  , NP.Seq 9 11
  , NP.Var 10 "NOP_10"
  , NP.Seq 10 11
  , NP.Var 11 "IF_ELSE_FOOTER"
  , NP.Var 12 "next_instr"
  , NP.Seq 12 13
  , NP.Var 13 "stack_pointer"
  , NP.Seq 13 14
  , NP.Assign 14 "undefed" (Num 0)
  , NP.Seq 14 15
  , NP.Assign 15 "undefed" (Num 0)
  , NP.Seq 15 16
  , NP.Assign 16 "undefed" (Num 0)
  , NP.Seq 16 17
  , NP.Assign 17 "undefed" (Num 0)
  , NP.Seq 17 18
  , NP.Assign 18 "undefed" (Num 0)
  , NP.Seq 18 19
  , NP.Assign 19 "undefed" (Num 0)
  , NP.Seq 19 20
  , NP.Assign 20 "undefed" (Num 0)
  , NP.Seq 20 21
  , NP.Assign 21 "undefed" (Num 0)
  , NP.Seq 21 22
  , NP.Assign 22 "undefed" (Num 0)
  , NP.Seq 22 23
  , NP.Assign 23 "undefed" (Num 0)
  , NP.Seq 23 24
  , NP.Assign 24 "undefed" (Num 0)
  , NP.Seq 24 25
  , NP.Branch 25 (Eq (Id "throwflag") (Num 1)) 27 33
  , NP.Branch 27 (Eq (Num 0) (Num 1)) 29 29
  , NP.Seq 28 1902
  , NP.Seq 28 30
  , NP.Var 29 "NOP_29"
  , NP.Seq 29 30
  , NP.Var 30 "IF_ELSE_FOOTER"
  , NP.Assign 31 "next_instr" (Num 0)
  , NP.Seq 31 32
  , NP.Assign 32 "stack_pointer" (Num 0)
  , NP.Seq 32 33
  , NP.Seq 32 1817
  , NP.Seq 32 34
  , NP.Var 33 "NOP_33"
  , NP.Seq 33 34
  , NP.Var 34 "IF_ELSE_FOOTER"
  , NP.Seq 34 1886
  , NP.Var 35 "__CLABEL_dispatch_opcode"
  , NP.Seq 35 36
  , NP.Branch 36 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 38 87
  , NP.Var 38 "NOP_38"
  , NP.Var 39 "__CLABEL_TARGET_BINARY_OP"
  , NP.Seq 39 40
  , NP.Assign 40 "undefed" (Num 0)
  , NP.Seq 40 41
  , NP.Assign 41 "next_instr" (Num 0)
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
  , NP.Assign 48 "rhs" (Num 0)
  , NP.Seq 48 49
  , NP.Assign 49 "lhs" (Num 0)
  , NP.Seq 49 50
  , NP.Var 50 "counter"
  , NP.Seq 50 51
  , NP.Branch 51 (Eq (Num 0) (Num 1)) 53 56
  , NP.Assign 53 "next_instr" (Num 0)
  , NP.Seq 53 54
  , NP.Assign 54 "stack_pointer" (Num 0)
  , NP.Seq 54 55
  , NP.Assign 55 "opcode" (Num 0)
  , NP.Seq 55 56
  , NP.Seq 55 35
  , NP.Seq 55 57
  , NP.Var 56 "NOP_56"
  , NP.Seq 56 57
  , NP.Var 57 "IF_ELSE_FOOTER"
  , NP.Assign 58 "undefed" (Num 0)
  , NP.Seq 58 59
  , NP.Branch 59 (Eq (Num 0) (Num 1)) 60 61
  , NP.Assign 60 "undefed" (Num 0)
  , NP.Seq 60 59
  , NP.Var 61 "LOOP_FOOTER"
  , NP.Seq 61 62
  , NP.Var 62 "lhs_o"
  , NP.Seq 62 63
  , NP.Var 63 "rhs_o"
  , NP.Seq 63 64
  , NP.Var 64 "res_o"
  , NP.Seq 64 65
  , NP.Assign 65 "stack_pointer" (Num 0)
  , NP.Seq 65 66
  , NP.Branch 66 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 68 68
  , NP.Seq 67 1817
  , NP.Seq 67 69
  , NP.Var 68 "NOP_68"
  , NP.Seq 68 69
  , NP.Var 69 "IF_ELSE_FOOTER"
  , NP.Assign 70 "res" (Num 0)
  , NP.Seq 70 71
  , NP.Var 71 "tmp"
  , NP.Seq 71 72
  , NP.Assign 72 "lhs" (Num 0)
  , NP.Seq 72 73
  , NP.Assign 73 "undefed" (Num 0)
  , NP.Seq 73 74
  , NP.Assign 74 "tmp" (Num 0)
  , NP.Seq 74 75
  , NP.Assign 75 "rhs" (Num 0)
  , NP.Seq 75 76
  , NP.Assign 76 "undefed" (Num 0)
  , NP.Seq 76 77
  , NP.Assign 77 "stack_pointer" (Num 0)
  , NP.Seq 77 78
  , NP.Assign 78 "stack_pointer" (Num 0)
  , NP.Seq 78 79
  , NP.Var 79 "word"
  , NP.Seq 79 80
  , NP.Assign 80 "opcode" (Num 0)
  , NP.Seq 80 81
  , NP.Assign 81 "oparg" (Num 0)
  , NP.Seq 81 82
  , NP.Branch 82 (Eq (Num 0) (Num 1)) 83 86
  , NP.Var 83 "word"
  , NP.Seq 83 84
  , NP.Assign 84 "opcode" (Num 0)
  , NP.Seq 84 85
  , NP.Assign 85 "oparg" (Num 0)
  , NP.Seq 85 86
  , NP.Seq 85 82
  , NP.Var 86 "LOOP_FOOTER"
  , NP.Seq 86 87
  , NP.Seq 86 35
  , NP.Branch 87 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 89 129
  , NP.Var 89 "NOP_89"
  , NP.Var 90 "__CLABEL_TARGET_BINARY_OP_ADD_FLOAT"
  , NP.Seq 90 91
  , NP.Var 91 "this_instr"
  , NP.Seq 91 92
  , NP.Assign 92 "undefed" (Num 0)
  , NP.Seq 92 93
  , NP.Assign 93 "next_instr" (Num 0)
  , NP.Seq 93 94
  , NP.Var 94 "value"
  , NP.Seq 94 95
  , NP.Var 95 "left"
  , NP.Seq 95 96
  , NP.Var 96 "right"
  , NP.Seq 96 97
  , NP.Var 97 "res"
  , NP.Seq 97 98
  , NP.Assign 98 "value" (Num 0)
  , NP.Seq 98 99
  , NP.Var 99 "value_o"
  , NP.Seq 99 100
  , NP.Branch 100 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 102 102
  , NP.Seq 101 42
  , NP.Seq 101 103
  , NP.Var 102 "NOP_102"
  , NP.Seq 102 103
  , NP.Var 103 "IF_ELSE_FOOTER"
  , NP.Assign 104 "left" (Num 0)
  , NP.Seq 104 105
  , NP.Var 105 "left_o"
  , NP.Seq 105 106
  , NP.Branch 106 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 108 108
  , NP.Seq 107 42
  , NP.Seq 107 109
  , NP.Var 108 "NOP_108"
  , NP.Seq 108 109
  , NP.Var 109 "IF_ELSE_FOOTER"
  , NP.Assign 110 "right" (Num 0)
  , NP.Seq 110 111
  , NP.Var 111 "left_o"
  , NP.Seq 111 112
  , NP.Var 112 "right_o"
  , NP.Seq 112 113
  , NP.Var 113 "dres"
  , NP.Seq 113 114
  , NP.Assign 114 "res" (Num 0)
  , NP.Seq 114 115
  , NP.Branch 115 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 117 117
  , NP.Seq 116 1813
  , NP.Seq 116 118
  , NP.Var 117 "NOP_117"
  , NP.Seq 117 118
  , NP.Var 118 "IF_ELSE_FOOTER"
  , NP.Assign 119 "undefed" (Num 0)
  , NP.Seq 119 120
  , NP.Assign 120 "stack_pointer" (Num 0)
  , NP.Seq 120 121
  , NP.Var 121 "word"
  , NP.Seq 121 122
  , NP.Assign 122 "opcode" (Num 0)
  , NP.Seq 122 123
  , NP.Assign 123 "oparg" (Num 0)
  , NP.Seq 123 124
  , NP.Branch 124 (Eq (Num 0) (Num 1)) 125 128
  , NP.Var 125 "word"
  , NP.Seq 125 126
  , NP.Assign 126 "opcode" (Num 0)
  , NP.Seq 126 127
  , NP.Assign 127 "oparg" (Num 0)
  , NP.Seq 127 128
  , NP.Seq 127 124
  , NP.Var 128 "LOOP_FOOTER"
  , NP.Seq 128 129
  , NP.Seq 128 35
  , NP.Branch 129 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 131 170
  , NP.Var 131 "NOP_131"
  , NP.Var 132 "__CLABEL_TARGET_BINARY_OP_ADD_INT"
  , NP.Seq 132 133
  , NP.Var 133 "this_instr"
  , NP.Seq 133 134
  , NP.Assign 134 "undefed" (Num 0)
  , NP.Seq 134 135
  , NP.Assign 135 "next_instr" (Num 0)
  , NP.Seq 135 136
  , NP.Var 136 "value"
  , NP.Seq 136 137
  , NP.Var 137 "left"
  , NP.Seq 137 138
  , NP.Var 138 "right"
  , NP.Seq 138 139
  , NP.Var 139 "res"
  , NP.Seq 139 140
  , NP.Assign 140 "value" (Num 0)
  , NP.Seq 140 141
  , NP.Var 141 "value_o"
  , NP.Seq 141 142
  , NP.Branch 142 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 144 144
  , NP.Seq 143 42
  , NP.Seq 143 145
  , NP.Var 144 "NOP_144"
  , NP.Seq 144 145
  , NP.Var 145 "IF_ELSE_FOOTER"
  , NP.Assign 146 "left" (Num 0)
  , NP.Seq 146 147
  , NP.Var 147 "left_o"
  , NP.Seq 147 148
  , NP.Branch 148 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 150 150
  , NP.Seq 149 42
  , NP.Seq 149 151
  , NP.Var 150 "NOP_150"
  , NP.Seq 150 151
  , NP.Var 151 "IF_ELSE_FOOTER"
  , NP.Assign 152 "right" (Num 0)
  , NP.Seq 152 153
  , NP.Var 153 "left_o"
  , NP.Seq 153 154
  , NP.Var 154 "right_o"
  , NP.Seq 154 155
  , NP.Assign 155 "res" (Num 0)
  , NP.Seq 155 156
  , NP.Branch 156 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 158 158
  , NP.Seq 157 42
  , NP.Seq 157 159
  , NP.Var 158 "NOP_158"
  , NP.Seq 158 159
  , NP.Var 159 "IF_ELSE_FOOTER"
  , NP.Assign 160 "undefed" (Num 0)
  , NP.Seq 160 161
  , NP.Assign 161 "stack_pointer" (Num 0)
  , NP.Seq 161 162
  , NP.Var 162 "word"
  , NP.Seq 162 163
  , NP.Assign 163 "opcode" (Num 0)
  , NP.Seq 163 164
  , NP.Assign 164 "oparg" (Num 0)
  , NP.Seq 164 165
  , NP.Branch 165 (Eq (Num 0) (Num 1)) 166 169
  , NP.Var 166 "word"
  , NP.Seq 166 167
  , NP.Assign 167 "opcode" (Num 0)
  , NP.Seq 167 168
  , NP.Assign 168 "oparg" (Num 0)
  , NP.Seq 168 169
  , NP.Seq 168 165
  , NP.Var 169 "LOOP_FOOTER"
  , NP.Seq 169 170
  , NP.Seq 169 35
  , NP.Branch 170 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 172 214
  , NP.Var 172 "NOP_172"
  , NP.Var 173 "__CLABEL_TARGET_BINARY_OP_ADD_UNICODE"
  , NP.Seq 173 174
  , NP.Var 174 "this_instr"
  , NP.Seq 174 175
  , NP.Assign 175 "undefed" (Num 0)
  , NP.Seq 175 176
  , NP.Assign 176 "next_instr" (Num 0)
  , NP.Seq 176 177
  , NP.Var 177 "value"
  , NP.Seq 177 178
  , NP.Var 178 "nos"
  , NP.Seq 178 179
  , NP.Var 179 "left"
  , NP.Seq 179 180
  , NP.Var 180 "right"
  , NP.Seq 180 181
  , NP.Var 181 "res"
  , NP.Seq 181 182
  , NP.Assign 182 "value" (Num 0)
  , NP.Seq 182 183
  , NP.Var 183 "value_o"
  , NP.Seq 183 184
  , NP.Branch 184 (Eq (Plus (Plus (Num 0) (Plus (Id "PyUnicode_Type") (Num 0))) (Num 0)) (Num 1)) 186 186
  , NP.Seq 185 42
  , NP.Seq 185 187
  , NP.Var 186 "NOP_186"
  , NP.Seq 186 187
  , NP.Var 187 "IF_ELSE_FOOTER"
  , NP.Assign 188 "nos" (Num 0)
  , NP.Seq 188 189
  , NP.Var 189 "o"
  , NP.Seq 189 190
  , NP.Branch 190 (Eq (Plus (Plus (Num 0) (Plus (Id "PyUnicode_Type") (Num 0))) (Num 0)) (Num 1)) 192 192
  , NP.Seq 191 42
  , NP.Seq 191 193
  , NP.Var 192 "NOP_192"
  , NP.Seq 192 193
  , NP.Var 193 "IF_ELSE_FOOTER"
  , NP.Assign 194 "right" (Num 0)
  , NP.Seq 194 195
  , NP.Assign 195 "left" (Num 0)
  , NP.Seq 195 196
  , NP.Var 196 "left_o"
  , NP.Seq 196 197
  , NP.Var 197 "right_o"
  , NP.Seq 197 198
  , NP.Var 198 "res_o"
  , NP.Seq 198 199
  , NP.Branch 199 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 201 201
  , NP.Seq 200 1813
  , NP.Seq 200 202
  , NP.Var 201 "NOP_201"
  , NP.Seq 201 202
  , NP.Var 202 "IF_ELSE_FOOTER"
  , NP.Assign 203 "res" (Num 0)
  , NP.Seq 203 204
  , NP.Assign 204 "undefed" (Num 0)
  , NP.Seq 204 205
  , NP.Assign 205 "stack_pointer" (Num 0)
  , NP.Seq 205 206
  , NP.Var 206 "word"
  , NP.Seq 206 207
  , NP.Assign 207 "opcode" (Num 0)
  , NP.Seq 207 208
  , NP.Assign 208 "oparg" (Num 0)
  , NP.Seq 208 209
  , NP.Branch 209 (Eq (Num 0) (Num 1)) 210 213
  , NP.Var 210 "word"
  , NP.Seq 210 211
  , NP.Assign 211 "opcode" (Num 0)
  , NP.Seq 211 212
  , NP.Assign 212 "oparg" (Num 0)
  , NP.Seq 212 213
  , NP.Seq 212 209
  , NP.Var 213 "LOOP_FOOTER"
  , NP.Seq 213 214
  , NP.Seq 213 35
  , NP.Branch 214 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 216 260
  , NP.Var 216 "NOP_216"
  , NP.Var 217 "__CLABEL_TARGET_BINARY_OP_EXTEND"
  , NP.Seq 217 218
  , NP.Var 218 "this_instr"
  , NP.Seq 218 219
  , NP.Assign 219 "undefed" (Num 0)
  , NP.Seq 219 220
  , NP.Assign 220 "next_instr" (Num 0)
  , NP.Seq 220 221
  , NP.Var 221 "left"
  , NP.Seq 221 222
  , NP.Var 222 "right"
  , NP.Seq 222 223
  , NP.Var 223 "res"
  , NP.Seq 223 224
  , NP.Assign 224 "right" (Num 0)
  , NP.Seq 224 225
  , NP.Assign 225 "left" (Num 0)
  , NP.Seq 225 226
  , NP.Var 226 "descr"
  , NP.Seq 226 227
  , NP.Var 227 "left_o"
  , NP.Seq 227 228
  , NP.Var 228 "right_o"
  , NP.Seq 228 229
  , NP.Var 229 "d"
  , NP.Seq 229 230
  , NP.Var 230 "res"
  , NP.Seq 230 231
  , NP.Assign 231 "stack_pointer" (Num 0)
  , NP.Seq 231 232
  , NP.Branch 232 (Eq (Plus (Id "res") (Num 0)) (Num 1)) 234 234
  , NP.Seq 233 42
  , NP.Seq 233 235
  , NP.Var 234 "NOP_234"
  , NP.Seq 234 235
  , NP.Var 235 "IF_ELSE_FOOTER"
  , NP.Var 236 "descr"
  , NP.Seq 236 237
  , NP.Var 237 "left_o"
  , NP.Seq 237 238
  , NP.Var 238 "right_o"
  , NP.Seq 238 239
  , NP.Var 239 "d"
  , NP.Seq 239 240
  , NP.Var 240 "res_o"
  , NP.Seq 240 241
  , NP.Var 241 "tmp"
  , NP.Seq 241 242
  , NP.Assign 242 "right" (Num 0)
  , NP.Seq 242 243
  , NP.Assign 243 "undefed" (Num 0)
  , NP.Seq 243 244
  , NP.Assign 244 "tmp" (Num 0)
  , NP.Seq 244 245
  , NP.Assign 245 "left" (Num 0)
  , NP.Seq 245 246
  , NP.Assign 246 "undefed" (Num 0)
  , NP.Seq 246 247
  , NP.Assign 247 "stack_pointer" (Num 0)
  , NP.Seq 247 248
  , NP.Assign 248 "stack_pointer" (Num 0)
  , NP.Seq 248 249
  , NP.Assign 249 "res" (Num 0)
  , NP.Seq 249 250
  , NP.Assign 250 "undefed" (Num 0)
  , NP.Seq 250 251
  , NP.Assign 251 "stack_pointer" (Num 0)
  , NP.Seq 251 252
  , NP.Var 252 "word"
  , NP.Seq 252 253
  , NP.Assign 253 "opcode" (Num 0)
  , NP.Seq 253 254
  , NP.Assign 254 "oparg" (Num 0)
  , NP.Seq 254 255
  , NP.Branch 255 (Eq (Num 0) (Num 1)) 256 259
  , NP.Var 256 "word"
  , NP.Seq 256 257
  , NP.Assign 257 "opcode" (Num 0)
  , NP.Seq 257 258
  , NP.Assign 258 "oparg" (Num 0)
  , NP.Seq 258 259
  , NP.Seq 258 255
  , NP.Var 259 "LOOP_FOOTER"
  , NP.Seq 259 260
  , NP.Seq 259 35
  , NP.Branch 260 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 262 354
  , NP.Var 262 "NOP_262"
  , NP.Var 263 "__CLABEL_TARGET_BINARY_OP_INPLACE_ADD_UNICODE"
  , NP.Seq 263 264
  , NP.Var 264 "this_instr"
  , NP.Seq 264 265
  , NP.Assign 265 "undefed" (Num 0)
  , NP.Seq 265 266
  , NP.Assign 266 "next_instr" (Num 0)
  , NP.Seq 266 267
  , NP.Var 267 "value"
  , NP.Seq 267 268
  , NP.Var 268 "nos"
  , NP.Seq 268 269
  , NP.Var 269 "left"
  , NP.Seq 269 270
  , NP.Var 270 "right"
  , NP.Seq 270 271
  , NP.Assign 271 "value" (Num 0)
  , NP.Seq 271 272
  , NP.Var 272 "value_o"
  , NP.Seq 272 273
  , NP.Branch 273 (Eq (Plus (Plus (Num 0) (Plus (Id "PyUnicode_Type") (Num 0))) (Num 0)) (Num 1)) 275 275
  , NP.Seq 274 42
  , NP.Seq 274 276
  , NP.Var 275 "NOP_275"
  , NP.Seq 275 276
  , NP.Var 276 "IF_ELSE_FOOTER"
  , NP.Assign 277 "nos" (Num 0)
  , NP.Seq 277 278
  , NP.Var 278 "o"
  , NP.Seq 278 279
  , NP.Branch 279 (Eq (Plus (Plus (Num 0) (Plus (Id "PyUnicode_Type") (Num 0))) (Num 0)) (Num 1)) 281 281
  , NP.Seq 280 42
  , NP.Seq 280 282
  , NP.Var 281 "NOP_281"
  , NP.Seq 281 282
  , NP.Var 282 "IF_ELSE_FOOTER"
  , NP.Assign 283 "right" (Num 0)
  , NP.Seq 283 284
  , NP.Assign 284 "left" (Num 0)
  , NP.Seq 284 285
  , NP.Var 285 "left_o"
  , NP.Seq 285 286
  , NP.Var 286 "next_oparg"
  , NP.Seq 286 287
  , NP.Assign 287 "next_oparg" (Num 0)
  , NP.Seq 287 288
  , NP.Var 288 "target_local"
  , NP.Seq 288 289
  , NP.Branch 289 (Eq (Plus (Num 0) (Id "left_o")) (Num 1)) 291 291
  , NP.Seq 290 42
  , NP.Seq 290 292
  , NP.Var 291 "NOP_291"
  , NP.Seq 291 292
  , NP.Var 292 "IF_ELSE_FOOTER"
  , NP.Var 293 "temp"
  , NP.Seq 293 294
  , NP.Var 294 "right_o"
  , NP.Seq 294 295
  , NP.Assign 295 "stack_pointer" (Num 0)
  , NP.Seq 295 296
  , NP.Assign 296 "stack_pointer" (Num 0)
  , NP.Seq 296 297
  , NP.Assign 297 "undefed" (Num 0)
  , NP.Seq 297 298
  , NP.Var 298 "op"
  , NP.Seq 298 299
  , NP.Branch 299 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 301 316
  , NP.Var 301 "tracer"
  , NP.Seq 301 302
  , NP.Branch 302 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 304 305
  , NP.Var 304 "data"
  , NP.Seq 304 305
  , NP.Seq 304 306
  , NP.Var 305 "NOP_305"
  , NP.Seq 305 306
  , NP.Var 306 "IF_ELSE_FOOTER"
  , NP.Branch 307 (Eq (Num 0) (Num 1)) 308 314
  , NP.Var 308 "tracer"
  , NP.Seq 308 309
  , NP.Branch 309 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 311 312
  , NP.Var 311 "data"
  , NP.Seq 311 312
  , NP.Seq 311 313
  , NP.Var 312 "NOP_312"
  , NP.Seq 312 313
  , NP.Var 313 "IF_ELSE_FOOTER"
  , NP.Seq 313 307
  , NP.Var 314 "LOOP_FOOTER"
  , NP.Seq 314 315
  , NP.Var 315 "dealloc"
  , NP.Seq 315 316
  , NP.Seq 315 317
  , NP.Var 316 "NOP_316"
  , NP.Seq 316 317
  , NP.Var 317 "IF_ELSE_FOOTER"
  , NP.Branch 318 (Eq (Num 0) (Num 1)) 319 339
  , NP.Var 319 "op"
  , NP.Seq 319 320
  , NP.Branch 320 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 322 337
  , NP.Var 322 "tracer"
  , NP.Seq 322 323
  , NP.Branch 323 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 325 326
  , NP.Var 325 "data"
  , NP.Seq 325 326
  , NP.Seq 325 327
  , NP.Var 326 "NOP_326"
  , NP.Seq 326 327
  , NP.Var 327 "IF_ELSE_FOOTER"
  , NP.Branch 328 (Eq (Num 0) (Num 1)) 329 335
  , NP.Var 329 "tracer"
  , NP.Seq 329 330
  , NP.Branch 330 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 332 333
  , NP.Var 332 "data"
  , NP.Seq 332 333
  , NP.Seq 332 334
  , NP.Var 333 "NOP_333"
  , NP.Seq 333 334
  , NP.Var 334 "IF_ELSE_FOOTER"
  , NP.Seq 334 328
  , NP.Var 335 "LOOP_FOOTER"
  , NP.Seq 335 336
  , NP.Var 336 "dealloc"
  , NP.Seq 336 337
  , NP.Seq 336 338
  , NP.Var 337 "NOP_337"
  , NP.Seq 337 338
  , NP.Var 338 "IF_ELSE_FOOTER"
  , NP.Seq 338 318
  , NP.Var 339 "LOOP_FOOTER"
  , NP.Seq 339 340
  , NP.Assign 340 "stack_pointer" (Num 0)
  , NP.Seq 340 341
  , NP.Branch 341 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 343 343
  , NP.Seq 342 1817
  , NP.Seq 342 344
  , NP.Var 343 "NOP_343"
  , NP.Seq 343 344
  , NP.Var 344 "IF_ELSE_FOOTER"
  , NP.Assign 345 "next_instr" (Num 0)
  , NP.Seq 345 346
  , NP.Var 346 "word"
  , NP.Seq 346 347
  , NP.Assign 347 "opcode" (Num 0)
  , NP.Seq 347 348
  , NP.Assign 348 "oparg" (Num 0)
  , NP.Seq 348 349
  , NP.Branch 349 (Eq (Num 0) (Num 1)) 350 353
  , NP.Var 350 "word"
  , NP.Seq 350 351
  , NP.Assign 351 "opcode" (Num 0)
  , NP.Seq 351 352
  , NP.Assign 352 "oparg" (Num 0)
  , NP.Seq 352 353
  , NP.Seq 352 349
  , NP.Var 353 "LOOP_FOOTER"
  , NP.Seq 353 354
  , NP.Seq 353 35
  , NP.Branch 354 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 356 396
  , NP.Var 356 "NOP_356"
  , NP.Var 357 "__CLABEL_TARGET_BINARY_OP_MULTIPLY_FLOAT"
  , NP.Seq 357 358
  , NP.Var 358 "this_instr"
  , NP.Seq 358 359
  , NP.Assign 359 "undefed" (Num 0)
  , NP.Seq 359 360
  , NP.Assign 360 "next_instr" (Num 0)
  , NP.Seq 360 361
  , NP.Var 361 "value"
  , NP.Seq 361 362
  , NP.Var 362 "left"
  , NP.Seq 362 363
  , NP.Var 363 "right"
  , NP.Seq 363 364
  , NP.Var 364 "res"
  , NP.Seq 364 365
  , NP.Assign 365 "value" (Num 0)
  , NP.Seq 365 366
  , NP.Var 366 "value_o"
  , NP.Seq 366 367
  , NP.Branch 367 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 369 369
  , NP.Seq 368 42
  , NP.Seq 368 370
  , NP.Var 369 "NOP_369"
  , NP.Seq 369 370
  , NP.Var 370 "IF_ELSE_FOOTER"
  , NP.Assign 371 "left" (Num 0)
  , NP.Seq 371 372
  , NP.Var 372 "left_o"
  , NP.Seq 372 373
  , NP.Branch 373 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 375 375
  , NP.Seq 374 42
  , NP.Seq 374 376
  , NP.Var 375 "NOP_375"
  , NP.Seq 375 376
  , NP.Var 376 "IF_ELSE_FOOTER"
  , NP.Assign 377 "right" (Num 0)
  , NP.Seq 377 378
  , NP.Var 378 "left_o"
  , NP.Seq 378 379
  , NP.Var 379 "right_o"
  , NP.Seq 379 380
  , NP.Var 380 "dres"
  , NP.Seq 380 381
  , NP.Assign 381 "res" (Num 0)
  , NP.Seq 381 382
  , NP.Branch 382 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 384 384
  , NP.Seq 383 1813
  , NP.Seq 383 385
  , NP.Var 384 "NOP_384"
  , NP.Seq 384 385
  , NP.Var 385 "IF_ELSE_FOOTER"
  , NP.Assign 386 "undefed" (Num 0)
  , NP.Seq 386 387
  , NP.Assign 387 "stack_pointer" (Num 0)
  , NP.Seq 387 388
  , NP.Var 388 "word"
  , NP.Seq 388 389
  , NP.Assign 389 "opcode" (Num 0)
  , NP.Seq 389 390
  , NP.Assign 390 "oparg" (Num 0)
  , NP.Seq 390 391
  , NP.Branch 391 (Eq (Num 0) (Num 1)) 392 395
  , NP.Var 392 "word"
  , NP.Seq 392 393
  , NP.Assign 393 "opcode" (Num 0)
  , NP.Seq 393 394
  , NP.Assign 394 "oparg" (Num 0)
  , NP.Seq 394 395
  , NP.Seq 394 391
  , NP.Var 395 "LOOP_FOOTER"
  , NP.Seq 395 396
  , NP.Seq 395 35
  , NP.Branch 396 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 398 437
  , NP.Var 398 "NOP_398"
  , NP.Var 399 "__CLABEL_TARGET_BINARY_OP_MULTIPLY_INT"
  , NP.Seq 399 400
  , NP.Var 400 "this_instr"
  , NP.Seq 400 401
  , NP.Assign 401 "undefed" (Num 0)
  , NP.Seq 401 402
  , NP.Assign 402 "next_instr" (Num 0)
  , NP.Seq 402 403
  , NP.Var 403 "value"
  , NP.Seq 403 404
  , NP.Var 404 "left"
  , NP.Seq 404 405
  , NP.Var 405 "right"
  , NP.Seq 405 406
  , NP.Var 406 "res"
  , NP.Seq 406 407
  , NP.Assign 407 "value" (Num 0)
  , NP.Seq 407 408
  , NP.Var 408 "value_o"
  , NP.Seq 408 409
  , NP.Branch 409 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 411 411
  , NP.Seq 410 42
  , NP.Seq 410 412
  , NP.Var 411 "NOP_411"
  , NP.Seq 411 412
  , NP.Var 412 "IF_ELSE_FOOTER"
  , NP.Assign 413 "left" (Num 0)
  , NP.Seq 413 414
  , NP.Var 414 "left_o"
  , NP.Seq 414 415
  , NP.Branch 415 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 417 417
  , NP.Seq 416 42
  , NP.Seq 416 418
  , NP.Var 417 "NOP_417"
  , NP.Seq 417 418
  , NP.Var 418 "IF_ELSE_FOOTER"
  , NP.Assign 419 "right" (Num 0)
  , NP.Seq 419 420
  , NP.Var 420 "left_o"
  , NP.Seq 420 421
  , NP.Var 421 "right_o"
  , NP.Seq 421 422
  , NP.Assign 422 "res" (Num 0)
  , NP.Seq 422 423
  , NP.Branch 423 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 425 425
  , NP.Seq 424 42
  , NP.Seq 424 426
  , NP.Var 425 "NOP_425"
  , NP.Seq 425 426
  , NP.Var 426 "IF_ELSE_FOOTER"
  , NP.Assign 427 "undefed" (Num 0)
  , NP.Seq 427 428
  , NP.Assign 428 "stack_pointer" (Num 0)
  , NP.Seq 428 429
  , NP.Var 429 "word"
  , NP.Seq 429 430
  , NP.Assign 430 "opcode" (Num 0)
  , NP.Seq 430 431
  , NP.Assign 431 "oparg" (Num 0)
  , NP.Seq 431 432
  , NP.Branch 432 (Eq (Num 0) (Num 1)) 433 436
  , NP.Var 433 "word"
  , NP.Seq 433 434
  , NP.Assign 434 "opcode" (Num 0)
  , NP.Seq 434 435
  , NP.Assign 435 "oparg" (Num 0)
  , NP.Seq 435 436
  , NP.Seq 435 432
  , NP.Var 436 "LOOP_FOOTER"
  , NP.Seq 436 437
  , NP.Seq 436 35
  , NP.Branch 437 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 439 489
  , NP.Var 439 "NOP_439"
  , NP.Var 440 "__CLABEL_TARGET_BINARY_OP_SUBSCR_DICT"
  , NP.Seq 440 441
  , NP.Var 441 "this_instr"
  , NP.Seq 441 442
  , NP.Assign 442 "undefed" (Num 0)
  , NP.Seq 442 443
  , NP.Assign 443 "next_instr" (Num 0)
  , NP.Seq 443 444
  , NP.Var 444 "nos"
  , NP.Seq 444 445
  , NP.Var 445 "dict_st"
  , NP.Seq 445 446
  , NP.Var 446 "sub_st"
  , NP.Seq 446 447
  , NP.Var 447 "res"
  , NP.Seq 447 448
  , NP.Assign 448 "nos" (Num 0)
  , NP.Seq 448 449
  , NP.Var 449 "o"
  , NP.Seq 449 450
  , NP.Branch 450 (Eq (Plus (Plus (Num 0) (Plus (Id "PyDict_Type") (Num 0))) (Num 0)) (Num 1)) 452 452
  , NP.Seq 451 42
  , NP.Seq 451 453
  , NP.Var 452 "NOP_452"
  , NP.Seq 452 453
  , NP.Var 453 "IF_ELSE_FOOTER"
  , NP.Assign 454 "sub_st" (Num 0)
  , NP.Seq 454 455
  , NP.Assign 455 "dict_st" (Num 0)
  , NP.Seq 455 456
  , NP.Var 456 "sub"
  , NP.Seq 456 457
  , NP.Var 457 "dict"
  , NP.Seq 457 458
  , NP.Var 458 "res_o"
  , NP.Seq 458 459
  , NP.Var 459 "rc"
  , NP.Seq 459 460
  , NP.Assign 460 "stack_pointer" (Num 0)
  , NP.Seq 460 461
  , NP.Branch 461 (Eq (Plus (Id "rc") (Num 0)) (Num 1)) 463 464
  , NP.Assign 463 "stack_pointer" (Num 0)
  , NP.Seq 463 465
  , NP.Var 464 "NOP_464"
  , NP.Seq 464 465
  , NP.Var 465 "IF_ELSE_FOOTER"
  , NP.Var 466 "tmp"
  , NP.Seq 466 467
  , NP.Assign 467 "sub_st" (Num 0)
  , NP.Seq 467 468
  , NP.Assign 468 "undefed" (Num 0)
  , NP.Seq 468 469
  , NP.Assign 469 "tmp" (Num 0)
  , NP.Seq 469 470
  , NP.Assign 470 "dict_st" (Num 0)
  , NP.Seq 470 471
  , NP.Assign 471 "undefed" (Num 0)
  , NP.Seq 471 472
  , NP.Assign 472 "stack_pointer" (Num 0)
  , NP.Seq 472 473
  , NP.Assign 473 "stack_pointer" (Num 0)
  , NP.Seq 473 474
  , NP.Branch 474 (Eq (Plus (Id "rc") (Num 0)) (Num 1)) 476 476
  , NP.Seq 475 1817
  , NP.Seq 475 477
  , NP.Var 476 "NOP_476"
  , NP.Seq 476 477
  , NP.Var 477 "IF_ELSE_FOOTER"
  , NP.Assign 478 "res" (Num 0)
  , NP.Seq 478 479
  , NP.Assign 479 "undefed" (Num 0)
  , NP.Seq 479 480
  , NP.Assign 480 "stack_pointer" (Num 0)
  , NP.Seq 480 481
  , NP.Var 481 "word"
  , NP.Seq 481 482
  , NP.Assign 482 "opcode" (Num 0)
  , NP.Seq 482 483
  , NP.Assign 483 "oparg" (Num 0)
  , NP.Seq 483 484
  , NP.Branch 484 (Eq (Num 0) (Num 1)) 485 488
  , NP.Var 485 "word"
  , NP.Seq 485 486
  , NP.Assign 486 "opcode" (Num 0)
  , NP.Seq 486 487
  , NP.Assign 487 "oparg" (Num 0)
  , NP.Seq 487 488
  , NP.Seq 487 484
  , NP.Var 488 "LOOP_FOOTER"
  , NP.Seq 488 489
  , NP.Seq 488 35
  , NP.Branch 489 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 491 549
  , NP.Var 491 "NOP_491"
  , NP.Var 492 "__CLABEL_TARGET_BINARY_OP_SUBSCR_GETITEM"
  , NP.Seq 492 493
  , NP.Var 493 "this_instr"
  , NP.Seq 493 494
  , NP.Assign 494 "undefed" (Num 0)
  , NP.Seq 494 495
  , NP.Assign 495 "next_instr" (Num 0)
  , NP.Seq 495 496
  , NP.Var 496 "container"
  , NP.Seq 496 497
  , NP.Var 497 "getitem"
  , NP.Seq 497 498
  , NP.Var 498 "sub"
  , NP.Seq 498 499
  , NP.Var 499 "new_frame"
  , NP.Seq 499 500
  , NP.Branch 500 (Eq (Num 0) (Num 1)) 502 502
  , NP.Seq 501 42
  , NP.Seq 501 503
  , NP.Var 502 "NOP_502"
  , NP.Seq 502 503
  , NP.Var 503 "IF_ELSE_FOOTER"
  , NP.Assign 504 "container" (Num 0)
  , NP.Seq 504 505
  , NP.Var 505 "tp"
  , NP.Seq 505 506
  , NP.Branch 506 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 508 508
  , NP.Seq 507 42
  , NP.Seq 507 509
  , NP.Var 508 "NOP_508"
  , NP.Seq 508 509
  , NP.Var 509 "IF_ELSE_FOOTER"
  , NP.Var 510 "ht"
  , NP.Seq 510 511
  , NP.Var 511 "getitem_o"
  , NP.Seq 511 512
  , NP.Branch 512 (Eq (Plus (Id "getitem_o") (Num 0)) (Num 1)) 514 514
  , NP.Seq 513 42
  , NP.Seq 513 515
  , NP.Var 514 "NOP_514"
  , NP.Seq 514 515
  , NP.Var 515 "IF_ELSE_FOOTER"
  , NP.Var 516 "cached_version"
  , NP.Seq 516 517
  , NP.Branch 517 (Eq (Plus (Num 0) (Id "cached_version")) (Num 1)) 519 519
  , NP.Seq 518 42
  , NP.Seq 518 520
  , NP.Var 519 "NOP_519"
  , NP.Seq 519 520
  , NP.Var 520 "IF_ELSE_FOOTER"
  , NP.Var 521 "code"
  , NP.Seq 521 522
  , NP.Branch 522 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 524 524
  , NP.Seq 523 42
  , NP.Seq 523 525
  , NP.Var 524 "NOP_524"
  , NP.Seq 524 525
  , NP.Var 525 "IF_ELSE_FOOTER"
  , NP.Assign 526 "getitem" (Num 0)
  , NP.Seq 526 527
  , NP.Assign 527 "sub" (Num 0)
  , NP.Seq 527 528
  , NP.Var 528 "pushed_frame"
  , NP.Seq 528 529
  , NP.Assign 529 "undefed" (Num 0)
  , NP.Seq 529 530
  , NP.Assign 530 "undefed" (Num 0)
  , NP.Seq 530 531
  , NP.Assign 531 "undefed" (Num 0)
  , NP.Seq 531 532
  , NP.Assign 532 "new_frame" (Num 0)
  , NP.Seq 532 533
  , NP.Var 533 "temp"
  , NP.Seq 533 534
  , NP.Assign 534 "stack_pointer" (Num 0)
  , NP.Seq 534 535
  , NP.Assign 535 "frame" (Num 0)
  , NP.Seq 535 536
  , NP.Assign 536 "stack_pointer" (Num 0)
  , NP.Seq 536 537
  , NP.Assign 537 "next_instr" (Num 0)
  , NP.Seq 537 538
  , NP.Branch 538 (Eq (Num 0) (Num 1)) 539 540
  , NP.Assign 539 "next_instr" (Num 0)
  , NP.Seq 539 538
  , NP.Var 540 "LOOP_FOOTER"
  , NP.Seq 540 541
  , NP.Var 541 "word"
  , NP.Seq 541 542
  , NP.Assign 542 "opcode" (Num 0)
  , NP.Seq 542 543
  , NP.Assign 543 "oparg" (Num 0)
  , NP.Seq 543 544
  , NP.Branch 544 (Eq (Num 0) (Num 1)) 545 548
  , NP.Var 545 "word"
  , NP.Seq 545 546
  , NP.Assign 546 "opcode" (Num 0)
  , NP.Seq 546 547
  , NP.Assign 547 "oparg" (Num 0)
  , NP.Seq 547 548
  , NP.Seq 547 544
  , NP.Var 548 "LOOP_FOOTER"
  , NP.Seq 548 549
  , NP.Seq 548 35
  , NP.Branch 549 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 551 604
  , NP.Var 551 "NOP_551"
  , NP.Var 552 "__CLABEL_TARGET_BINARY_OP_SUBSCR_LIST_INT"
  , NP.Seq 552 553
  , NP.Var 553 "this_instr"
  , NP.Seq 553 554
  , NP.Assign 554 "undefed" (Num 0)
  , NP.Seq 554 555
  , NP.Assign 555 "next_instr" (Num 0)
  , NP.Seq 555 556
  , NP.Var 556 "value"
  , NP.Seq 556 557
  , NP.Var 557 "nos"
  , NP.Seq 557 558
  , NP.Var 558 "list_st"
  , NP.Seq 558 559
  , NP.Var 559 "sub_st"
  , NP.Seq 559 560
  , NP.Var 560 "res"
  , NP.Seq 560 561
  , NP.Assign 561 "value" (Num 0)
  , NP.Seq 561 562
  , NP.Var 562 "value_o"
  , NP.Seq 562 563
  , NP.Branch 563 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 565 565
  , NP.Seq 564 42
  , NP.Seq 564 566
  , NP.Var 565 "NOP_565"
  , NP.Seq 565 566
  , NP.Var 566 "IF_ELSE_FOOTER"
  , NP.Assign 567 "nos" (Num 0)
  , NP.Seq 567 568
  , NP.Var 568 "o"
  , NP.Seq 568 569
  , NP.Branch 569 (Eq (Plus (Plus (Num 0) (Plus (Id "PyList_Type") (Num 0))) (Num 0)) (Num 1)) 571 571
  , NP.Seq 570 42
  , NP.Seq 570 572
  , NP.Var 571 "NOP_571"
  , NP.Seq 571 572
  , NP.Var 572 "IF_ELSE_FOOTER"
  , NP.Assign 573 "sub_st" (Num 0)
  , NP.Seq 573 574
  , NP.Assign 574 "list_st" (Num 0)
  , NP.Seq 574 575
  , NP.Var 575 "sub"
  , NP.Seq 575 576
  , NP.Var 576 "list"
  , NP.Seq 576 577
  , NP.Branch 577 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 579 579
  , NP.Seq 578 42
  , NP.Seq 578 580
  , NP.Var 579 "NOP_579"
  , NP.Seq 579 580
  , NP.Var 580 "IF_ELSE_FOOTER"
  , NP.Var 581 "index"
  , NP.Seq 581 582
  , NP.Branch 582 (Eq (Plus (Id "index") (Num 0)) (Num 1)) 584 584
  , NP.Seq 583 42
  , NP.Seq 583 585
  , NP.Var 584 "NOP_584"
  , NP.Seq 584 585
  , NP.Var 585 "IF_ELSE_FOOTER"
  , NP.Var 586 "res_o"
  , NP.Seq 586 587
  , NP.Assign 587 "res" (Num 0)
  , NP.Seq 587 588
  , NP.Var 588 "tmp"
  , NP.Seq 588 589
  , NP.Assign 589 "list_st" (Num 0)
  , NP.Seq 589 590
  , NP.Assign 590 "undefed" (Num 0)
  , NP.Seq 590 591
  , NP.Assign 591 "tmp" (Num 0)
  , NP.Seq 591 592
  , NP.Assign 592 "sub_st" (Num 0)
  , NP.Seq 592 593
  , NP.Assign 593 "undefed" (Num 0)
  , NP.Seq 593 594
  , NP.Assign 594 "stack_pointer" (Num 0)
  , NP.Seq 594 595
  , NP.Assign 595 "stack_pointer" (Num 0)
  , NP.Seq 595 596
  , NP.Var 596 "word"
  , NP.Seq 596 597
  , NP.Assign 597 "opcode" (Num 0)
  , NP.Seq 597 598
  , NP.Assign 598 "oparg" (Num 0)
  , NP.Seq 598 599
  , NP.Branch 599 (Eq (Num 0) (Num 1)) 600 603
  , NP.Var 600 "word"
  , NP.Seq 600 601
  , NP.Assign 601 "opcode" (Num 0)
  , NP.Seq 601 602
  , NP.Assign 602 "oparg" (Num 0)
  , NP.Seq 602 603
  , NP.Seq 602 599
  , NP.Var 603 "LOOP_FOOTER"
  , NP.Seq 603 604
  , NP.Seq 603 35
  , NP.Branch 604 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 606 657
  , NP.Var 606 "NOP_606"
  , NP.Var 607 "__CLABEL_TARGET_BINARY_OP_SUBSCR_LIST_SLICE"
  , NP.Seq 607 608
  , NP.Var 608 "this_instr"
  , NP.Seq 608 609
  , NP.Assign 609 "undefed" (Num 0)
  , NP.Seq 609 610
  , NP.Assign 610 "next_instr" (Num 0)
  , NP.Seq 610 611
  , NP.Var 611 "tos"
  , NP.Seq 611 612
  , NP.Var 612 "nos"
  , NP.Seq 612 613
  , NP.Var 613 "list_st"
  , NP.Seq 613 614
  , NP.Var 614 "sub_st"
  , NP.Seq 614 615
  , NP.Var 615 "res"
  , NP.Seq 615 616
  , NP.Assign 616 "tos" (Num 0)
  , NP.Seq 616 617
  , NP.Var 617 "o"
  , NP.Seq 617 618
  , NP.Branch 618 (Eq (Plus (Plus (Num 0) (Plus (Id "PySlice_Type") (Num 0))) (Num 0)) (Num 1)) 620 620
  , NP.Seq 619 42
  , NP.Seq 619 621
  , NP.Var 620 "NOP_620"
  , NP.Seq 620 621
  , NP.Var 621 "IF_ELSE_FOOTER"
  , NP.Assign 622 "nos" (Num 0)
  , NP.Seq 622 623
  , NP.Var 623 "o"
  , NP.Seq 623 624
  , NP.Branch 624 (Eq (Plus (Plus (Num 0) (Plus (Id "PyList_Type") (Num 0))) (Num 0)) (Num 1)) 626 626
  , NP.Seq 625 42
  , NP.Seq 625 627
  , NP.Var 626 "NOP_626"
  , NP.Seq 626 627
  , NP.Var 627 "IF_ELSE_FOOTER"
  , NP.Assign 628 "sub_st" (Num 0)
  , NP.Seq 628 629
  , NP.Assign 629 "list_st" (Num 0)
  , NP.Seq 629 630
  , NP.Var 630 "sub"
  , NP.Seq 630 631
  , NP.Var 631 "list"
  , NP.Seq 631 632
  , NP.Var 632 "res_o"
  , NP.Seq 632 633
  , NP.Assign 633 "stack_pointer" (Num 0)
  , NP.Seq 633 634
  , NP.Var 634 "tmp"
  , NP.Seq 634 635
  , NP.Assign 635 "sub_st" (Num 0)
  , NP.Seq 635 636
  , NP.Assign 636 "undefed" (Num 0)
  , NP.Seq 636 637
  , NP.Assign 637 "tmp" (Num 0)
  , NP.Seq 637 638
  , NP.Assign 638 "list_st" (Num 0)
  , NP.Seq 638 639
  , NP.Assign 639 "undefed" (Num 0)
  , NP.Seq 639 640
  , NP.Assign 640 "stack_pointer" (Num 0)
  , NP.Seq 640 641
  , NP.Assign 641 "stack_pointer" (Num 0)
  , NP.Seq 641 642
  , NP.Branch 642 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 644 644
  , NP.Seq 643 1817
  , NP.Seq 643 645
  , NP.Var 644 "NOP_644"
  , NP.Seq 644 645
  , NP.Var 645 "IF_ELSE_FOOTER"
  , NP.Assign 646 "res" (Num 0)
  , NP.Seq 646 647
  , NP.Assign 647 "undefed" (Num 0)
  , NP.Seq 647 648
  , NP.Assign 648 "stack_pointer" (Num 0)
  , NP.Seq 648 649
  , NP.Var 649 "word"
  , NP.Seq 649 650
  , NP.Assign 650 "opcode" (Num 0)
  , NP.Seq 650 651
  , NP.Assign 651 "oparg" (Num 0)
  , NP.Seq 651 652
  , NP.Branch 652 (Eq (Num 0) (Num 1)) 653 656
  , NP.Var 653 "word"
  , NP.Seq 653 654
  , NP.Assign 654 "opcode" (Num 0)
  , NP.Seq 654 655
  , NP.Assign 655 "oparg" (Num 0)
  , NP.Seq 655 656
  , NP.Seq 655 652
  , NP.Var 656 "LOOP_FOOTER"
  , NP.Seq 656 657
  , NP.Seq 656 35
  , NP.Branch 657 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 659 713
  , NP.Var 659 "NOP_659"
  , NP.Var 660 "__CLABEL_TARGET_BINARY_OP_SUBSCR_STR_INT"
  , NP.Seq 660 661
  , NP.Var 661 "this_instr"
  , NP.Seq 661 662
  , NP.Assign 662 "undefed" (Num 0)
  , NP.Seq 662 663
  , NP.Assign 663 "next_instr" (Num 0)
  , NP.Seq 663 664
  , NP.Var 664 "value"
  , NP.Seq 664 665
  , NP.Var 665 "nos"
  , NP.Seq 665 666
  , NP.Var 666 "str_st"
  , NP.Seq 666 667
  , NP.Var 667 "sub_st"
  , NP.Seq 667 668
  , NP.Var 668 "res"
  , NP.Seq 668 669
  , NP.Assign 669 "value" (Num 0)
  , NP.Seq 669 670
  , NP.Var 670 "value_o"
  , NP.Seq 670 671
  , NP.Branch 671 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 673 673
  , NP.Seq 672 42
  , NP.Seq 672 674
  , NP.Var 673 "NOP_673"
  , NP.Seq 673 674
  , NP.Var 674 "IF_ELSE_FOOTER"
  , NP.Assign 675 "nos" (Num 0)
  , NP.Seq 675 676
  , NP.Var 676 "o"
  , NP.Seq 676 677
  , NP.Branch 677 (Eq (Plus (Plus (Num 0) (Plus (Id "PyUnicode_Type") (Num 0))) (Num 0)) (Num 1)) 679 679
  , NP.Seq 678 42
  , NP.Seq 678 680
  , NP.Var 679 "NOP_679"
  , NP.Seq 679 680
  , NP.Var 680 "IF_ELSE_FOOTER"
  , NP.Assign 681 "sub_st" (Num 0)
  , NP.Seq 681 682
  , NP.Assign 682 "str_st" (Num 0)
  , NP.Seq 682 683
  , NP.Var 683 "sub"
  , NP.Seq 683 684
  , NP.Var 684 "str"
  , NP.Seq 684 685
  , NP.Branch 685 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 687 687
  , NP.Seq 686 42
  , NP.Seq 686 688
  , NP.Var 687 "NOP_687"
  , NP.Seq 687 688
  , NP.Var 688 "IF_ELSE_FOOTER"
  , NP.Var 689 "index"
  , NP.Seq 689 690
  , NP.Branch 690 (Eq (Plus (Num 0) (Id "index")) (Num 1)) 692 692
  , NP.Seq 691 42
  , NP.Seq 691 693
  , NP.Var 692 "NOP_692"
  , NP.Seq 692 693
  , NP.Var 693 "IF_ELSE_FOOTER"
  , NP.Var 694 "c"
  , NP.Seq 694 695
  , NP.Branch 695 (Eq (Plus (Plus (Plus (Num 0) (Num 0)) (Plus (Num 0) (Num 0))) (Id "c")) (Num 1)) 697 697
  , NP.Seq 696 42
  , NP.Seq 696 698
  , NP.Var 697 "NOP_697"
  , NP.Seq 697 698
  , NP.Var 698 "IF_ELSE_FOOTER"
  , NP.Var 699 "res_o"
  , NP.Seq 699 700
  , NP.Assign 700 "stack_pointer" (Num 0)
  , NP.Seq 700 701
  , NP.Assign 701 "stack_pointer" (Num 0)
  , NP.Seq 701 702
  , NP.Assign 702 "res" (Num 0)
  , NP.Seq 702 703
  , NP.Assign 703 "undefed" (Num 0)
  , NP.Seq 703 704
  , NP.Assign 704 "stack_pointer" (Num 0)
  , NP.Seq 704 705
  , NP.Var 705 "word"
  , NP.Seq 705 706
  , NP.Assign 706 "opcode" (Num 0)
  , NP.Seq 706 707
  , NP.Assign 707 "oparg" (Num 0)
  , NP.Seq 707 708
  , NP.Branch 708 (Eq (Num 0) (Num 1)) 709 712
  , NP.Var 709 "word"
  , NP.Seq 709 710
  , NP.Assign 710 "opcode" (Num 0)
  , NP.Seq 710 711
  , NP.Assign 711 "oparg" (Num 0)
  , NP.Seq 711 712
  , NP.Seq 711 708
  , NP.Var 712 "LOOP_FOOTER"
  , NP.Seq 712 713
  , NP.Seq 712 35
  , NP.Branch 713 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 715 765
  , NP.Var 715 "NOP_715"
  , NP.Var 716 "__CLABEL_TARGET_BINARY_OP_SUBSCR_TUPLE_INT"
  , NP.Seq 716 717
  , NP.Var 717 "this_instr"
  , NP.Seq 717 718
  , NP.Assign 718 "undefed" (Num 0)
  , NP.Seq 718 719
  , NP.Assign 719 "next_instr" (Num 0)
  , NP.Seq 719 720
  , NP.Var 720 "value"
  , NP.Seq 720 721
  , NP.Var 721 "nos"
  , NP.Seq 721 722
  , NP.Var 722 "tuple_st"
  , NP.Seq 722 723
  , NP.Var 723 "sub_st"
  , NP.Seq 723 724
  , NP.Var 724 "res"
  , NP.Seq 724 725
  , NP.Assign 725 "value" (Num 0)
  , NP.Seq 725 726
  , NP.Var 726 "value_o"
  , NP.Seq 726 727
  , NP.Branch 727 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 729 729
  , NP.Seq 728 42
  , NP.Seq 728 730
  , NP.Var 729 "NOP_729"
  , NP.Seq 729 730
  , NP.Var 730 "IF_ELSE_FOOTER"
  , NP.Assign 731 "nos" (Num 0)
  , NP.Seq 731 732
  , NP.Var 732 "o"
  , NP.Seq 732 733
  , NP.Branch 733 (Eq (Plus (Plus (Num 0) (Plus (Id "PyTuple_Type") (Num 0))) (Num 0)) (Num 1)) 735 735
  , NP.Seq 734 42
  , NP.Seq 734 736
  , NP.Var 735 "NOP_735"
  , NP.Seq 735 736
  , NP.Var 736 "IF_ELSE_FOOTER"
  , NP.Assign 737 "sub_st" (Num 0)
  , NP.Seq 737 738
  , NP.Assign 738 "tuple_st" (Num 0)
  , NP.Seq 738 739
  , NP.Var 739 "sub"
  , NP.Seq 739 740
  , NP.Var 740 "tuple"
  , NP.Seq 740 741
  , NP.Branch 741 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 743 743
  , NP.Seq 742 42
  , NP.Seq 742 744
  , NP.Var 743 "NOP_743"
  , NP.Seq 743 744
  , NP.Var 744 "IF_ELSE_FOOTER"
  , NP.Var 745 "index"
  , NP.Seq 745 746
  , NP.Branch 746 (Eq (Plus (Id "index") (Num 0)) (Num 1)) 748 748
  , NP.Seq 747 42
  , NP.Seq 747 749
  , NP.Var 748 "NOP_748"
  , NP.Seq 748 749
  , NP.Var 749 "IF_ELSE_FOOTER"
  , NP.Var 750 "res_o"
  , NP.Seq 750 751
  , NP.Assign 751 "res" (Num 0)
  , NP.Seq 751 752
  , NP.Assign 752 "stack_pointer" (Num 0)
  , NP.Seq 752 753
  , NP.Var 753 "tmp"
  , NP.Seq 753 754
  , NP.Assign 754 "tuple_st" (Num 0)
  , NP.Seq 754 755
  , NP.Assign 755 "undefed" (Num 0)
  , NP.Seq 755 756
  , NP.Assign 756 "stack_pointer" (Num 0)
  , NP.Seq 756 757
  , NP.Var 757 "word"
  , NP.Seq 757 758
  , NP.Assign 758 "opcode" (Num 0)
  , NP.Seq 758 759
  , NP.Assign 759 "oparg" (Num 0)
  , NP.Seq 759 760
  , NP.Branch 760 (Eq (Num 0) (Num 1)) 761 764
  , NP.Var 761 "word"
  , NP.Seq 761 762
  , NP.Assign 762 "opcode" (Num 0)
  , NP.Seq 762 763
  , NP.Assign 763 "oparg" (Num 0)
  , NP.Seq 763 764
  , NP.Seq 763 760
  , NP.Var 764 "LOOP_FOOTER"
  , NP.Seq 764 765
  , NP.Seq 764 35
  , NP.Branch 765 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 767 807
  , NP.Var 767 "NOP_767"
  , NP.Var 768 "__CLABEL_TARGET_BINARY_OP_SUBTRACT_FLOAT"
  , NP.Seq 768 769
  , NP.Var 769 "this_instr"
  , NP.Seq 769 770
  , NP.Assign 770 "undefed" (Num 0)
  , NP.Seq 770 771
  , NP.Assign 771 "next_instr" (Num 0)
  , NP.Seq 771 772
  , NP.Var 772 "value"
  , NP.Seq 772 773
  , NP.Var 773 "left"
  , NP.Seq 773 774
  , NP.Var 774 "right"
  , NP.Seq 774 775
  , NP.Var 775 "res"
  , NP.Seq 775 776
  , NP.Assign 776 "value" (Num 0)
  , NP.Seq 776 777
  , NP.Var 777 "value_o"
  , NP.Seq 777 778
  , NP.Branch 778 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 780 780
  , NP.Seq 779 42
  , NP.Seq 779 781
  , NP.Var 780 "NOP_780"
  , NP.Seq 780 781
  , NP.Var 781 "IF_ELSE_FOOTER"
  , NP.Assign 782 "left" (Num 0)
  , NP.Seq 782 783
  , NP.Var 783 "left_o"
  , NP.Seq 783 784
  , NP.Branch 784 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 786 786
  , NP.Seq 785 42
  , NP.Seq 785 787
  , NP.Var 786 "NOP_786"
  , NP.Seq 786 787
  , NP.Var 787 "IF_ELSE_FOOTER"
  , NP.Assign 788 "right" (Num 0)
  , NP.Seq 788 789
  , NP.Var 789 "left_o"
  , NP.Seq 789 790
  , NP.Var 790 "right_o"
  , NP.Seq 790 791
  , NP.Var 791 "dres"
  , NP.Seq 791 792
  , NP.Assign 792 "res" (Num 0)
  , NP.Seq 792 793
  , NP.Branch 793 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 795 795
  , NP.Seq 794 1813
  , NP.Seq 794 796
  , NP.Var 795 "NOP_795"
  , NP.Seq 795 796
  , NP.Var 796 "IF_ELSE_FOOTER"
  , NP.Assign 797 "undefed" (Num 0)
  , NP.Seq 797 798
  , NP.Assign 798 "stack_pointer" (Num 0)
  , NP.Seq 798 799
  , NP.Var 799 "word"
  , NP.Seq 799 800
  , NP.Assign 800 "opcode" (Num 0)
  , NP.Seq 800 801
  , NP.Assign 801 "oparg" (Num 0)
  , NP.Seq 801 802
  , NP.Branch 802 (Eq (Num 0) (Num 1)) 803 806
  , NP.Var 803 "word"
  , NP.Seq 803 804
  , NP.Assign 804 "opcode" (Num 0)
  , NP.Seq 804 805
  , NP.Assign 805 "oparg" (Num 0)
  , NP.Seq 805 806
  , NP.Seq 805 802
  , NP.Var 806 "LOOP_FOOTER"
  , NP.Seq 806 807
  , NP.Seq 806 35
  , NP.Branch 807 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 809 848
  , NP.Var 809 "NOP_809"
  , NP.Var 810 "__CLABEL_TARGET_BINARY_OP_SUBTRACT_INT"
  , NP.Seq 810 811
  , NP.Var 811 "this_instr"
  , NP.Seq 811 812
  , NP.Assign 812 "undefed" (Num 0)
  , NP.Seq 812 813
  , NP.Assign 813 "next_instr" (Num 0)
  , NP.Seq 813 814
  , NP.Var 814 "value"
  , NP.Seq 814 815
  , NP.Var 815 "left"
  , NP.Seq 815 816
  , NP.Var 816 "right"
  , NP.Seq 816 817
  , NP.Var 817 "res"
  , NP.Seq 817 818
  , NP.Assign 818 "value" (Num 0)
  , NP.Seq 818 819
  , NP.Var 819 "value_o"
  , NP.Seq 819 820
  , NP.Branch 820 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 822 822
  , NP.Seq 821 42
  , NP.Seq 821 823
  , NP.Var 822 "NOP_822"
  , NP.Seq 822 823
  , NP.Var 823 "IF_ELSE_FOOTER"
  , NP.Assign 824 "left" (Num 0)
  , NP.Seq 824 825
  , NP.Var 825 "left_o"
  , NP.Seq 825 826
  , NP.Branch 826 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 828 828
  , NP.Seq 827 42
  , NP.Seq 827 829
  , NP.Var 828 "NOP_828"
  , NP.Seq 828 829
  , NP.Var 829 "IF_ELSE_FOOTER"
  , NP.Assign 830 "right" (Num 0)
  , NP.Seq 830 831
  , NP.Var 831 "left_o"
  , NP.Seq 831 832
  , NP.Var 832 "right_o"
  , NP.Seq 832 833
  , NP.Assign 833 "res" (Num 0)
  , NP.Seq 833 834
  , NP.Branch 834 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 836 836
  , NP.Seq 835 42
  , NP.Seq 835 837
  , NP.Var 836 "NOP_836"
  , NP.Seq 836 837
  , NP.Var 837 "IF_ELSE_FOOTER"
  , NP.Assign 838 "undefed" (Num 0)
  , NP.Seq 838 839
  , NP.Assign 839 "stack_pointer" (Num 0)
  , NP.Seq 839 840
  , NP.Var 840 "word"
  , NP.Seq 840 841
  , NP.Assign 841 "opcode" (Num 0)
  , NP.Seq 841 842
  , NP.Assign 842 "oparg" (Num 0)
  , NP.Seq 842 843
  , NP.Branch 843 (Eq (Num 0) (Num 1)) 844 847
  , NP.Var 844 "word"
  , NP.Seq 844 845
  , NP.Assign 845 "opcode" (Num 0)
  , NP.Seq 845 846
  , NP.Assign 846 "oparg" (Num 0)
  , NP.Seq 846 847
  , NP.Seq 846 843
  , NP.Var 847 "LOOP_FOOTER"
  , NP.Seq 847 848
  , NP.Seq 847 35
  , NP.Branch 848 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 850 931
  , NP.Var 850 "NOP_850"
  , NP.Var 851 "__CLABEL_TARGET_BINARY_SLICE"
  , NP.Seq 851 852
  , NP.Assign 852 "undefed" (Num 0)
  , NP.Seq 852 853
  , NP.Assign 853 "next_instr" (Num 0)
  , NP.Seq 853 854
  , NP.Var 854 "container"
  , NP.Seq 854 855
  , NP.Var 855 "start"
  , NP.Seq 855 856
  , NP.Var 856 "stop"
  , NP.Seq 856 857
  , NP.Var 857 "res"
  , NP.Seq 857 858
  , NP.Assign 858 "stop" (Num 0)
  , NP.Seq 858 859
  , NP.Assign 859 "start" (Num 0)
  , NP.Seq 859 860
  , NP.Assign 860 "container" (Num 0)
  , NP.Seq 860 861
  , NP.Var 861 "slice"
  , NP.Seq 861 862
  , NP.Assign 862 "stack_pointer" (Num 0)
  , NP.Seq 862 863
  , NP.Var 863 "res_o"
  , NP.Seq 863 864
  , NP.Branch 864 (Eq (Plus (Id "slice") (Num 0)) (Num 1)) 866 867
  , NP.Assign 866 "res_o" (Num 0)
  , NP.Seq 866 913
  , NP.Assign 867 "stack_pointer" (Num 0)
  , NP.Seq 867 868
  , NP.Assign 868 "res_o" (Num 0)
  , NP.Seq 868 869
  , NP.Var 869 "op"
  , NP.Seq 869 870
  , NP.Branch 870 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 872 887
  , NP.Var 872 "tracer"
  , NP.Seq 872 873
  , NP.Branch 873 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 875 876
  , NP.Var 875 "data"
  , NP.Seq 875 876
  , NP.Seq 875 877
  , NP.Var 876 "NOP_876"
  , NP.Seq 876 877
  , NP.Var 877 "IF_ELSE_FOOTER"
  , NP.Branch 878 (Eq (Num 0) (Num 1)) 879 885
  , NP.Var 879 "tracer"
  , NP.Seq 879 880
  , NP.Branch 880 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 882 883
  , NP.Var 882 "data"
  , NP.Seq 882 883
  , NP.Seq 882 884
  , NP.Var 883 "NOP_883"
  , NP.Seq 883 884
  , NP.Var 884 "IF_ELSE_FOOTER"
  , NP.Seq 884 878
  , NP.Var 885 "LOOP_FOOTER"
  , NP.Seq 885 886
  , NP.Var 886 "dealloc"
  , NP.Seq 886 887
  , NP.Seq 886 888
  , NP.Var 887 "NOP_887"
  , NP.Seq 887 888
  , NP.Var 888 "IF_ELSE_FOOTER"
  , NP.Branch 889 (Eq (Num 0) (Num 1)) 890 910
  , NP.Var 890 "op"
  , NP.Seq 890 891
  , NP.Branch 891 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 893 908
  , NP.Var 893 "tracer"
  , NP.Seq 893 894
  , NP.Branch 894 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 896 897
  , NP.Var 896 "data"
  , NP.Seq 896 897
  , NP.Seq 896 898
  , NP.Var 897 "NOP_897"
  , NP.Seq 897 898
  , NP.Var 898 "IF_ELSE_FOOTER"
  , NP.Branch 899 (Eq (Num 0) (Num 1)) 900 906
  , NP.Var 900 "tracer"
  , NP.Seq 900 901
  , NP.Branch 901 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 903 904
  , NP.Var 903 "data"
  , NP.Seq 903 904
  , NP.Seq 903 905
  , NP.Var 904 "NOP_904"
  , NP.Seq 904 905
  , NP.Var 905 "IF_ELSE_FOOTER"
  , NP.Seq 905 899
  , NP.Var 906 "LOOP_FOOTER"
  , NP.Seq 906 907
  , NP.Var 907 "dealloc"
  , NP.Seq 907 908
  , NP.Seq 907 909
  , NP.Var 908 "NOP_908"
  , NP.Seq 908 909
  , NP.Var 909 "IF_ELSE_FOOTER"
  , NP.Seq 909 889
  , NP.Var 910 "LOOP_FOOTER"
  , NP.Seq 910 911
  , NP.Assign 911 "stack_pointer" (Num 0)
  , NP.Seq 911 912
  , NP.Assign 912 "stack_pointer" (Num 0)
  , NP.Seq 912 913
  , NP.Var 913 "IF_ELSE_FOOTER"
  , NP.Assign 914 "stack_pointer" (Num 0)
  , NP.Seq 914 915
  , NP.Assign 915 "stack_pointer" (Num 0)
  , NP.Seq 915 916
  , NP.Branch 916 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 918 918
  , NP.Seq 917 1817
  , NP.Seq 917 919
  , NP.Var 918 "NOP_918"
  , NP.Seq 918 919
  , NP.Var 919 "IF_ELSE_FOOTER"
  , NP.Assign 920 "res" (Num 0)
  , NP.Seq 920 921
  , NP.Assign 921 "undefed" (Num 0)
  , NP.Seq 921 922
  , NP.Assign 922 "stack_pointer" (Num 0)
  , NP.Seq 922 923
  , NP.Var 923 "word"
  , NP.Seq 923 924
  , NP.Assign 924 "opcode" (Num 0)
  , NP.Seq 924 925
  , NP.Assign 925 "oparg" (Num 0)
  , NP.Seq 925 926
  , NP.Branch 926 (Eq (Num 0) (Num 1)) 927 930
  , NP.Var 927 "word"
  , NP.Seq 927 928
  , NP.Assign 928 "opcode" (Num 0)
  , NP.Seq 928 929
  , NP.Assign 929 "oparg" (Num 0)
  , NP.Seq 929 930
  , NP.Seq 929 926
  , NP.Var 930 "LOOP_FOOTER"
  , NP.Seq 930 931
  , NP.Seq 930 35
  , NP.Branch 931 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 933 980
  , NP.Var 933 "NOP_933"
  , NP.Var 934 "__CLABEL_TARGET_BUILD_INTERPOLATION"
  , NP.Seq 934 935
  , NP.Assign 935 "undefed" (Num 0)
  , NP.Seq 935 936
  , NP.Assign 936 "next_instr" (Num 0)
  , NP.Seq 936 937
  , NP.Var 937 "value"
  , NP.Seq 937 938
  , NP.Var 938 "str"
  , NP.Seq 938 939
  , NP.Var 939 "format"
  , NP.Seq 939 940
  , NP.Var 940 "interpolation"
  , NP.Seq 940 941
  , NP.Assign 941 "format" (Num 0)
  , NP.Seq 941 942
  , NP.Assign 942 "str" (Num 0)
  , NP.Seq 942 943
  , NP.Assign 943 "value" (Num 0)
  , NP.Seq 943 944
  , NP.Var 944 "value_o"
  , NP.Seq 944 945
  , NP.Var 945 "str_o"
  , NP.Seq 945 946
  , NP.Var 946 "conversion"
  , NP.Seq 946 947
  , NP.Var 947 "format_o"
  , NP.Seq 947 948
  , NP.Branch 948 (Eq (Plus (Id "oparg") (Num 0)) (Num 1)) 950 951
  , NP.Assign 950 "format_o" (Num 0)
  , NP.Seq 950 952
  , NP.Assign 951 "format_o" (Num 0)
  , NP.Seq 951 952
  , NP.Var 952 "IF_ELSE_FOOTER"
  , NP.Var 953 "interpolation_o"
  , NP.Seq 953 954
  , NP.Assign 954 "stack_pointer" (Num 0)
  , NP.Seq 954 955
  , NP.Branch 955 (Eq (Plus (Id "oparg") (Num 0)) (Num 1)) 957 959
  , NP.Assign 957 "stack_pointer" (Num 0)
  , NP.Seq 957 958
  , NP.Assign 958 "stack_pointer" (Num 0)
  , NP.Seq 958 959
  , NP.Seq 958 960
  , NP.Assign 959 "stack_pointer" (Num 0)
  , NP.Seq 959 960
  , NP.Var 960 "IF_ELSE_FOOTER"
  , NP.Assign 961 "stack_pointer" (Num 0)
  , NP.Seq 961 962
  , NP.Assign 962 "stack_pointer" (Num 0)
  , NP.Seq 962 963
  , NP.Assign 963 "stack_pointer" (Num 0)
  , NP.Seq 963 964
  , NP.Assign 964 "stack_pointer" (Num 0)
  , NP.Seq 964 965
  , NP.Branch 965 (Eq (Plus (Id "interpolation_o") (Num 0)) (Num 1)) 967 967
  , NP.Seq 966 1817
  , NP.Seq 966 968
  , NP.Var 967 "NOP_967"
  , NP.Seq 967 968
  , NP.Var 968 "IF_ELSE_FOOTER"
  , NP.Assign 969 "interpolation" (Num 0)
  , NP.Seq 969 970
  , NP.Assign 970 "undefed" (Num 0)
  , NP.Seq 970 971
  , NP.Assign 971 "stack_pointer" (Num 0)
  , NP.Seq 971 972
  , NP.Var 972 "word"
  , NP.Seq 972 973
  , NP.Assign 973 "opcode" (Num 0)
  , NP.Seq 973 974
  , NP.Assign 974 "oparg" (Num 0)
  , NP.Seq 974 975
  , NP.Branch 975 (Eq (Num 0) (Num 1)) 976 979
  , NP.Var 976 "word"
  , NP.Seq 976 977
  , NP.Assign 977 "opcode" (Num 0)
  , NP.Seq 977 978
  , NP.Assign 978 "oparg" (Num 0)
  , NP.Seq 978 979
  , NP.Seq 978 975
  , NP.Var 979 "LOOP_FOOTER"
  , NP.Seq 979 980
  , NP.Seq 979 35
  , NP.Branch 980 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 982 1006
  , NP.Var 982 "NOP_982"
  , NP.Var 983 "__CLABEL_TARGET_BUILD_LIST"
  , NP.Seq 983 984
  , NP.Assign 984 "undefed" (Num 0)
  , NP.Seq 984 985
  , NP.Assign 985 "next_instr" (Num 0)
  , NP.Seq 985 986
  , NP.Var 986 "values"
  , NP.Seq 986 987
  , NP.Var 987 "list"
  , NP.Seq 987 988
  , NP.Assign 988 "values" (Num 0)
  , NP.Seq 988 989
  , NP.Var 989 "list_o"
  , NP.Seq 989 990
  , NP.Assign 990 "stack_pointer" (Num 0)
  , NP.Seq 990 991
  , NP.Branch 991 (Eq (Plus (Id "list_o") (Num 0)) (Num 1)) 993 993
  , NP.Seq 992 1817
  , NP.Seq 992 994
  , NP.Var 993 "NOP_993"
  , NP.Seq 993 994
  , NP.Var 994 "IF_ELSE_FOOTER"
  , NP.Assign 995 "list" (Num 0)
  , NP.Seq 995 996
  , NP.Assign 996 "undefed" (Num 0)
  , NP.Seq 996 997
  , NP.Assign 997 "stack_pointer" (Num 0)
  , NP.Seq 997 998
  , NP.Var 998 "word"
  , NP.Seq 998 999
  , NP.Assign 999 "opcode" (Num 0)
  , NP.Seq 999 1000
  , NP.Assign 1000 "oparg" (Num 0)
  , NP.Seq 1000 1001
  , NP.Branch 1001 (Eq (Num 0) (Num 1)) 1002 1005
  , NP.Var 1002 "word"
  , NP.Seq 1002 1003
  , NP.Assign 1003 "opcode" (Num 0)
  , NP.Seq 1003 1004
  , NP.Assign 1004 "oparg" (Num 0)
  , NP.Seq 1004 1005
  , NP.Seq 1004 1001
  , NP.Var 1005 "LOOP_FOOTER"
  , NP.Seq 1005 1006
  , NP.Seq 1005 35
  , NP.Branch 1006 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1008 1054
  , NP.Var 1008 "NOP_1008"
  , NP.Var 1009 "__CLABEL_TARGET_BUILD_MAP"
  , NP.Seq 1009 1010
  , NP.Assign 1010 "undefed" (Num 0)
  , NP.Seq 1010 1011
  , NP.Assign 1011 "next_instr" (Num 0)
  , NP.Seq 1011 1012
  , NP.Var 1012 "values"
  , NP.Seq 1012 1013
  , NP.Var 1013 "map"
  , NP.Seq 1013 1014
  , NP.Assign 1014 "values" (Num 0)
  , NP.Seq 1014 1015
  , NP.Var 1015 "values_o_temp"
  , NP.Seq 1015 1016
  , NP.Var 1016 "values_o"
  , NP.Seq 1016 1017
  , NP.Branch 1017 (Eq (Plus (Id "values_o") (Num 0)) (Num 1)) 1019 1027
  , NP.Var 1019 "tmp"
  , NP.Seq 1019 1020
  , NP.Var 1020 "_i"
  , NP.Seq 1020 1021
  , NP.Branch 1021 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1022 1024
  , NP.Assign 1022 "tmp" (Num 0)
  , NP.Seq 1022 1023
  , NP.Assign 1023 "undefed" (Num 0)
  , NP.Seq 1023 1024
  , NP.Seq 1023 1021
  , NP.Var 1024 "LOOP_FOOTER"
  , NP.Seq 1024 1025
  , NP.Assign 1025 "stack_pointer" (Num 0)
  , NP.Seq 1025 1026
  , NP.Assign 1026 "stack_pointer" (Num 0)
  , NP.Seq 1026 1027
  , NP.Seq 1026 1817
  , NP.Seq 1026 1028
  , NP.Var 1027 "NOP_1027"
  , NP.Seq 1027 1028
  , NP.Var 1028 "IF_ELSE_FOOTER"
  , NP.Var 1029 "map_o"
  , NP.Seq 1029 1030
  , NP.Assign 1030 "stack_pointer" (Num 0)
  , NP.Seq 1030 1031
  , NP.Var 1031 "tmp"
  , NP.Seq 1031 1032
  , NP.Var 1032 "_i"
  , NP.Seq 1032 1033
  , NP.Branch 1033 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1034 1036
  , NP.Assign 1034 "tmp" (Num 0)
  , NP.Seq 1034 1035
  , NP.Assign 1035 "undefed" (Num 0)
  , NP.Seq 1035 1036
  , NP.Seq 1035 1033
  , NP.Var 1036 "LOOP_FOOTER"
  , NP.Seq 1036 1037
  , NP.Assign 1037 "stack_pointer" (Num 0)
  , NP.Seq 1037 1038
  , NP.Assign 1038 "stack_pointer" (Num 0)
  , NP.Seq 1038 1039
  , NP.Branch 1039 (Eq (Plus (Id "map_o") (Num 0)) (Num 1)) 1041 1041
  , NP.Seq 1040 1817
  , NP.Seq 1040 1042
  , NP.Var 1041 "NOP_1041"
  , NP.Seq 1041 1042
  , NP.Var 1042 "IF_ELSE_FOOTER"
  , NP.Assign 1043 "map" (Num 0)
  , NP.Seq 1043 1044
  , NP.Assign 1044 "undefed" (Num 0)
  , NP.Seq 1044 1045
  , NP.Assign 1045 "stack_pointer" (Num 0)
  , NP.Seq 1045 1046
  , NP.Var 1046 "word"
  , NP.Seq 1046 1047
  , NP.Assign 1047 "opcode" (Num 0)
  , NP.Seq 1047 1048
  , NP.Assign 1048 "oparg" (Num 0)
  , NP.Seq 1048 1049
  , NP.Branch 1049 (Eq (Num 0) (Num 1)) 1050 1053
  , NP.Var 1050 "word"
  , NP.Seq 1050 1051
  , NP.Assign 1051 "opcode" (Num 0)
  , NP.Seq 1051 1052
  , NP.Assign 1052 "oparg" (Num 0)
  , NP.Seq 1052 1053
  , NP.Seq 1052 1049
  , NP.Var 1053 "LOOP_FOOTER"
  , NP.Seq 1053 1054
  , NP.Seq 1053 35
  , NP.Branch 1054 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1056 1148
  , NP.Var 1056 "NOP_1056"
  , NP.Var 1057 "__CLABEL_TARGET_BUILD_SET"
  , NP.Seq 1057 1058
  , NP.Assign 1058 "undefed" (Num 0)
  , NP.Seq 1058 1059
  , NP.Assign 1059 "next_instr" (Num 0)
  , NP.Seq 1059 1060
  , NP.Var 1060 "values"
  , NP.Seq 1060 1061
  , NP.Var 1061 "set"
  , NP.Seq 1061 1062
  , NP.Assign 1062 "values" (Num 0)
  , NP.Seq 1062 1063
  , NP.Var 1063 "set_o"
  , NP.Seq 1063 1064
  , NP.Assign 1064 "stack_pointer" (Num 0)
  , NP.Seq 1064 1065
  , NP.Branch 1065 (Eq (Plus (Id "set_o") (Num 0)) (Num 1)) 1067 1075
  , NP.Var 1067 "tmp"
  , NP.Seq 1067 1068
  , NP.Var 1068 "_i"
  , NP.Seq 1068 1069
  , NP.Branch 1069 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1070 1072
  , NP.Assign 1070 "tmp" (Num 0)
  , NP.Seq 1070 1071
  , NP.Assign 1071 "undefed" (Num 0)
  , NP.Seq 1071 1072
  , NP.Seq 1071 1069
  , NP.Var 1072 "LOOP_FOOTER"
  , NP.Seq 1072 1073
  , NP.Assign 1073 "stack_pointer" (Num 0)
  , NP.Seq 1073 1074
  , NP.Assign 1074 "stack_pointer" (Num 0)
  , NP.Seq 1074 1075
  , NP.Seq 1074 1817
  , NP.Seq 1074 1076
  , NP.Var 1075 "NOP_1075"
  , NP.Seq 1075 1076
  , NP.Var 1076 "IF_ELSE_FOOTER"
  , NP.Var 1077 "err"
  , NP.Seq 1077 1078
  , NP.Var 1078 "i"
  , NP.Seq 1078 1079
  , NP.Branch 1079 (Eq (Plus (Id "i") (Id "oparg")) (Num 1)) 1080 1088
  , NP.Var 1080 "value"
  , NP.Seq 1080 1081
  , NP.Assign 1081 "undefed" (Num 0)
  , NP.Seq 1081 1082
  , NP.Branch 1082 (Eq (Plus (Id "err") (Num 0)) (Num 1)) 1084 1086
  , NP.Assign 1084 "err" (Num 0)
  , NP.Seq 1084 1085
  , NP.Assign 1085 "stack_pointer" (Num 0)
  , NP.Seq 1085 1086
  , NP.Seq 1085 1087
  , NP.Assign 1086 "stack_pointer" (Num 0)
  , NP.Seq 1086 1087
  , NP.Var 1087 "IF_ELSE_FOOTER"
  , NP.Seq 1087 1079
  , NP.Var 1088 "LOOP_FOOTER"
  , NP.Seq 1088 1089
  , NP.Branch 1089 (Eq (Id "err") (Num 1)) 1091 1135
  , NP.Assign 1091 "stack_pointer" (Num 0)
  , NP.Seq 1091 1092
  , NP.Var 1092 "op"
  , NP.Seq 1092 1093
  , NP.Branch 1093 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1095 1110
  , NP.Var 1095 "tracer"
  , NP.Seq 1095 1096
  , NP.Branch 1096 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1098 1099
  , NP.Var 1098 "data"
  , NP.Seq 1098 1099
  , NP.Seq 1098 1100
  , NP.Var 1099 "NOP_1099"
  , NP.Seq 1099 1100
  , NP.Var 1100 "IF_ELSE_FOOTER"
  , NP.Branch 1101 (Eq (Num 0) (Num 1)) 1102 1108
  , NP.Var 1102 "tracer"
  , NP.Seq 1102 1103
  , NP.Branch 1103 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1105 1106
  , NP.Var 1105 "data"
  , NP.Seq 1105 1106
  , NP.Seq 1105 1107
  , NP.Var 1106 "NOP_1106"
  , NP.Seq 1106 1107
  , NP.Var 1107 "IF_ELSE_FOOTER"
  , NP.Seq 1107 1101
  , NP.Var 1108 "LOOP_FOOTER"
  , NP.Seq 1108 1109
  , NP.Var 1109 "dealloc"
  , NP.Seq 1109 1110
  , NP.Seq 1109 1111
  , NP.Var 1110 "NOP_1110"
  , NP.Seq 1110 1111
  , NP.Var 1111 "IF_ELSE_FOOTER"
  , NP.Branch 1112 (Eq (Num 0) (Num 1)) 1113 1133
  , NP.Var 1113 "op"
  , NP.Seq 1113 1114
  , NP.Branch 1114 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1116 1131
  , NP.Var 1116 "tracer"
  , NP.Seq 1116 1117
  , NP.Branch 1117 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1119 1120
  , NP.Var 1119 "data"
  , NP.Seq 1119 1120
  , NP.Seq 1119 1121
  , NP.Var 1120 "NOP_1120"
  , NP.Seq 1120 1121
  , NP.Var 1121 "IF_ELSE_FOOTER"
  , NP.Branch 1122 (Eq (Num 0) (Num 1)) 1123 1129
  , NP.Var 1123 "tracer"
  , NP.Seq 1123 1124
  , NP.Branch 1124 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1126 1127
  , NP.Var 1126 "data"
  , NP.Seq 1126 1127
  , NP.Seq 1126 1128
  , NP.Var 1127 "NOP_1127"
  , NP.Seq 1127 1128
  , NP.Var 1128 "IF_ELSE_FOOTER"
  , NP.Seq 1128 1122
  , NP.Var 1129 "LOOP_FOOTER"
  , NP.Seq 1129 1130
  , NP.Var 1130 "dealloc"
  , NP.Seq 1130 1131
  , NP.Seq 1130 1132
  , NP.Var 1131 "NOP_1131"
  , NP.Seq 1131 1132
  , NP.Var 1132 "IF_ELSE_FOOTER"
  , NP.Seq 1132 1112
  , NP.Var 1133 "LOOP_FOOTER"
  , NP.Seq 1133 1134
  , NP.Assign 1134 "stack_pointer" (Num 0)
  , NP.Seq 1134 1135
  , NP.Seq 1134 1817
  , NP.Seq 1134 1136
  , NP.Var 1135 "NOP_1135"
  , NP.Seq 1135 1136
  , NP.Var 1136 "IF_ELSE_FOOTER"
  , NP.Assign 1137 "set" (Num 0)
  , NP.Seq 1137 1138
  , NP.Assign 1138 "undefed" (Num 0)
  , NP.Seq 1138 1139
  , NP.Assign 1139 "stack_pointer" (Num 0)
  , NP.Seq 1139 1140
  , NP.Var 1140 "word"
  , NP.Seq 1140 1141
  , NP.Assign 1141 "opcode" (Num 0)
  , NP.Seq 1141 1142
  , NP.Assign 1142 "oparg" (Num 0)
  , NP.Seq 1142 1143
  , NP.Branch 1143 (Eq (Num 0) (Num 1)) 1144 1147
  , NP.Var 1144 "word"
  , NP.Seq 1144 1145
  , NP.Assign 1145 "opcode" (Num 0)
  , NP.Seq 1145 1146
  , NP.Assign 1146 "oparg" (Num 0)
  , NP.Seq 1146 1147
  , NP.Seq 1146 1143
  , NP.Var 1147 "LOOP_FOOTER"
  , NP.Seq 1147 1148
  , NP.Seq 1147 35
  , NP.Branch 1148 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1150 1184
  , NP.Var 1150 "NOP_1150"
  , NP.Var 1151 "__CLABEL_TARGET_BUILD_SLICE"
  , NP.Seq 1151 1152
  , NP.Assign 1152 "undefed" (Num 0)
  , NP.Seq 1152 1153
  , NP.Assign 1153 "next_instr" (Num 0)
  , NP.Seq 1153 1154
  , NP.Var 1154 "args"
  , NP.Seq 1154 1155
  , NP.Var 1155 "slice"
  , NP.Seq 1155 1156
  , NP.Assign 1156 "args" (Num 0)
  , NP.Seq 1156 1157
  , NP.Var 1157 "start_o"
  , NP.Seq 1157 1158
  , NP.Var 1158 "stop_o"
  , NP.Seq 1158 1159
  , NP.Var 1159 "step_o"
  , NP.Seq 1159 1160
  , NP.Var 1160 "slice_o"
  , NP.Seq 1160 1161
  , NP.Var 1161 "tmp"
  , NP.Seq 1161 1162
  , NP.Var 1162 "_i"
  , NP.Seq 1162 1163
  , NP.Branch 1163 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1164 1166
  , NP.Assign 1164 "tmp" (Num 0)
  , NP.Seq 1164 1165
  , NP.Assign 1165 "undefed" (Num 0)
  , NP.Seq 1165 1166
  , NP.Seq 1165 1163
  , NP.Var 1166 "LOOP_FOOTER"
  , NP.Seq 1166 1167
  , NP.Assign 1167 "stack_pointer" (Num 0)
  , NP.Seq 1167 1168
  , NP.Assign 1168 "stack_pointer" (Num 0)
  , NP.Seq 1168 1169
  , NP.Branch 1169 (Eq (Plus (Id "slice_o") (Num 0)) (Num 1)) 1171 1171
  , NP.Seq 1170 1817
  , NP.Seq 1170 1172
  , NP.Var 1171 "NOP_1171"
  , NP.Seq 1171 1172
  , NP.Var 1172 "IF_ELSE_FOOTER"
  , NP.Assign 1173 "slice" (Num 0)
  , NP.Seq 1173 1174
  , NP.Assign 1174 "undefed" (Num 0)
  , NP.Seq 1174 1175
  , NP.Assign 1175 "stack_pointer" (Num 0)
  , NP.Seq 1175 1176
  , NP.Var 1176 "word"
  , NP.Seq 1176 1177
  , NP.Assign 1177 "opcode" (Num 0)
  , NP.Seq 1177 1178
  , NP.Assign 1178 "oparg" (Num 0)
  , NP.Seq 1178 1179
  , NP.Branch 1179 (Eq (Num 0) (Num 1)) 1180 1183
  , NP.Var 1180 "word"
  , NP.Seq 1180 1181
  , NP.Assign 1181 "opcode" (Num 0)
  , NP.Seq 1181 1182
  , NP.Assign 1182 "oparg" (Num 0)
  , NP.Seq 1182 1183
  , NP.Seq 1182 1179
  , NP.Var 1183 "LOOP_FOOTER"
  , NP.Seq 1183 1184
  , NP.Seq 1183 35
  , NP.Branch 1184 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1186 1231
  , NP.Var 1186 "NOP_1186"
  , NP.Var 1187 "__CLABEL_TARGET_BUILD_STRING"
  , NP.Seq 1187 1188
  , NP.Assign 1188 "undefed" (Num 0)
  , NP.Seq 1188 1189
  , NP.Assign 1189 "next_instr" (Num 0)
  , NP.Seq 1189 1190
  , NP.Var 1190 "pieces"
  , NP.Seq 1190 1191
  , NP.Var 1191 "str"
  , NP.Seq 1191 1192
  , NP.Assign 1192 "pieces" (Num 0)
  , NP.Seq 1192 1193
  , NP.Var 1193 "pieces_o_temp"
  , NP.Seq 1193 1194
  , NP.Var 1194 "pieces_o"
  , NP.Seq 1194 1195
  , NP.Branch 1195 (Eq (Plus (Id "pieces_o") (Num 0)) (Num 1)) 1197 1205
  , NP.Var 1197 "tmp"
  , NP.Seq 1197 1198
  , NP.Var 1198 "_i"
  , NP.Seq 1198 1199
  , NP.Branch 1199 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1200 1202
  , NP.Assign 1200 "tmp" (Num 0)
  , NP.Seq 1200 1201
  , NP.Assign 1201 "undefed" (Num 0)
  , NP.Seq 1201 1202
  , NP.Seq 1201 1199
  , NP.Var 1202 "LOOP_FOOTER"
  , NP.Seq 1202 1203
  , NP.Assign 1203 "stack_pointer" (Num 0)
  , NP.Seq 1203 1204
  , NP.Assign 1204 "stack_pointer" (Num 0)
  , NP.Seq 1204 1205
  , NP.Seq 1204 1817
  , NP.Seq 1204 1206
  , NP.Var 1205 "NOP_1205"
  , NP.Seq 1205 1206
  , NP.Var 1206 "IF_ELSE_FOOTER"
  , NP.Var 1207 "str_o"
  , NP.Seq 1207 1208
  , NP.Var 1208 "tmp"
  , NP.Seq 1208 1209
  , NP.Var 1209 "_i"
  , NP.Seq 1209 1210
  , NP.Branch 1210 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1211 1213
  , NP.Assign 1211 "tmp" (Num 0)
  , NP.Seq 1211 1212
  , NP.Assign 1212 "undefed" (Num 0)
  , NP.Seq 1212 1213
  , NP.Seq 1212 1210
  , NP.Var 1213 "LOOP_FOOTER"
  , NP.Seq 1213 1214
  , NP.Assign 1214 "stack_pointer" (Num 0)
  , NP.Seq 1214 1215
  , NP.Assign 1215 "stack_pointer" (Num 0)
  , NP.Seq 1215 1216
  , NP.Branch 1216 (Eq (Plus (Id "str_o") (Num 0)) (Num 1)) 1218 1218
  , NP.Seq 1217 1817
  , NP.Seq 1217 1219
  , NP.Var 1218 "NOP_1218"
  , NP.Seq 1218 1219
  , NP.Var 1219 "IF_ELSE_FOOTER"
  , NP.Assign 1220 "str" (Num 0)
  , NP.Seq 1220 1221
  , NP.Assign 1221 "undefed" (Num 0)
  , NP.Seq 1221 1222
  , NP.Assign 1222 "stack_pointer" (Num 0)
  , NP.Seq 1222 1223
  , NP.Var 1223 "word"
  , NP.Seq 1223 1224
  , NP.Assign 1224 "opcode" (Num 0)
  , NP.Seq 1224 1225
  , NP.Assign 1225 "oparg" (Num 0)
  , NP.Seq 1225 1226
  , NP.Branch 1226 (Eq (Num 0) (Num 1)) 1227 1230
  , NP.Var 1227 "word"
  , NP.Seq 1227 1228
  , NP.Assign 1228 "opcode" (Num 0)
  , NP.Seq 1228 1229
  , NP.Assign 1229 "oparg" (Num 0)
  , NP.Seq 1229 1230
  , NP.Seq 1229 1226
  , NP.Var 1230 "LOOP_FOOTER"
  , NP.Seq 1230 1231
  , NP.Seq 1230 35
  , NP.Branch 1231 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1233 1265
  , NP.Var 1233 "NOP_1233"
  , NP.Var 1234 "__CLABEL_TARGET_BUILD_TEMPLATE"
  , NP.Seq 1234 1235
  , NP.Assign 1235 "undefed" (Num 0)
  , NP.Seq 1235 1236
  , NP.Assign 1236 "next_instr" (Num 0)
  , NP.Seq 1236 1237
  , NP.Var 1237 "strings"
  , NP.Seq 1237 1238
  , NP.Var 1238 "interpolations"
  , NP.Seq 1238 1239
  , NP.Var 1239 "template"
  , NP.Seq 1239 1240
  , NP.Assign 1240 "interpolations" (Num 0)
  , NP.Seq 1240 1241
  , NP.Assign 1241 "strings" (Num 0)
  , NP.Seq 1241 1242
  , NP.Var 1242 "strings_o"
  , NP.Seq 1242 1243
  , NP.Var 1243 "interpolations_o"
  , NP.Seq 1243 1244
  , NP.Var 1244 "template_o"
  , NP.Seq 1244 1245
  , NP.Assign 1245 "stack_pointer" (Num 0)
  , NP.Seq 1245 1246
  , NP.Assign 1246 "stack_pointer" (Num 0)
  , NP.Seq 1246 1247
  , NP.Assign 1247 "stack_pointer" (Num 0)
  , NP.Seq 1247 1248
  , NP.Assign 1248 "stack_pointer" (Num 0)
  , NP.Seq 1248 1249
  , NP.Assign 1249 "stack_pointer" (Num 0)
  , NP.Seq 1249 1250
  , NP.Branch 1250 (Eq (Plus (Id "template_o") (Num 0)) (Num 1)) 1252 1252
  , NP.Seq 1251 1817
  , NP.Seq 1251 1253
  , NP.Var 1252 "NOP_1252"
  , NP.Seq 1252 1253
  , NP.Var 1253 "IF_ELSE_FOOTER"
  , NP.Assign 1254 "template" (Num 0)
  , NP.Seq 1254 1255
  , NP.Assign 1255 "undefed" (Num 0)
  , NP.Seq 1255 1256
  , NP.Assign 1256 "stack_pointer" (Num 0)
  , NP.Seq 1256 1257
  , NP.Var 1257 "word"
  , NP.Seq 1257 1258
  , NP.Assign 1258 "opcode" (Num 0)
  , NP.Seq 1258 1259
  , NP.Assign 1259 "oparg" (Num 0)
  , NP.Seq 1259 1260
  , NP.Branch 1260 (Eq (Num 0) (Num 1)) 1261 1264
  , NP.Var 1261 "word"
  , NP.Seq 1261 1262
  , NP.Assign 1262 "opcode" (Num 0)
  , NP.Seq 1262 1263
  , NP.Assign 1263 "oparg" (Num 0)
  , NP.Seq 1263 1264
  , NP.Seq 1263 1260
  , NP.Var 1264 "LOOP_FOOTER"
  , NP.Seq 1264 1265
  , NP.Seq 1264 35
  , NP.Branch 1265 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1267 1290
  , NP.Var 1267 "NOP_1267"
  , NP.Var 1268 "__CLABEL_TARGET_BUILD_TUPLE"
  , NP.Seq 1268 1269
  , NP.Assign 1269 "undefed" (Num 0)
  , NP.Seq 1269 1270
  , NP.Assign 1270 "next_instr" (Num 0)
  , NP.Seq 1270 1271
  , NP.Var 1271 "values"
  , NP.Seq 1271 1272
  , NP.Var 1272 "tup"
  , NP.Seq 1272 1273
  , NP.Assign 1273 "values" (Num 0)
  , NP.Seq 1273 1274
  , NP.Var 1274 "tup_o"
  , NP.Seq 1274 1275
  , NP.Branch 1275 (Eq (Plus (Id "tup_o") (Num 0)) (Num 1)) 1277 1277
  , NP.Seq 1276 1817
  , NP.Seq 1276 1278
  , NP.Var 1277 "NOP_1277"
  , NP.Seq 1277 1278
  , NP.Var 1278 "IF_ELSE_FOOTER"
  , NP.Assign 1279 "tup" (Num 0)
  , NP.Seq 1279 1280
  , NP.Assign 1280 "undefed" (Num 0)
  , NP.Seq 1280 1281
  , NP.Assign 1281 "stack_pointer" (Num 0)
  , NP.Seq 1281 1282
  , NP.Var 1282 "word"
  , NP.Seq 1282 1283
  , NP.Assign 1283 "opcode" (Num 0)
  , NP.Seq 1283 1284
  , NP.Assign 1284 "oparg" (Num 0)
  , NP.Seq 1284 1285
  , NP.Branch 1285 (Eq (Num 0) (Num 1)) 1286 1289
  , NP.Var 1286 "word"
  , NP.Seq 1286 1287
  , NP.Assign 1287 "opcode" (Num 0)
  , NP.Seq 1287 1288
  , NP.Assign 1288 "oparg" (Num 0)
  , NP.Seq 1288 1289
  , NP.Seq 1288 1285
  , NP.Var 1289 "LOOP_FOOTER"
  , NP.Seq 1289 1290
  , NP.Seq 1289 35
  , NP.Branch 1290 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1292 1304
  , NP.Var 1292 "NOP_1292"
  , NP.Var 1293 "__CLABEL_TARGET_CACHE"
  , NP.Seq 1293 1294
  , NP.Assign 1294 "undefed" (Num 0)
  , NP.Seq 1294 1295
  , NP.Assign 1295 "next_instr" (Num 0)
  , NP.Seq 1295 1296
  , NP.Var 1296 "word"
  , NP.Seq 1296 1297
  , NP.Assign 1297 "opcode" (Num 0)
  , NP.Seq 1297 1298
  , NP.Assign 1298 "oparg" (Num 0)
  , NP.Seq 1298 1299
  , NP.Branch 1299 (Eq (Num 0) (Num 1)) 1300 1303
  , NP.Var 1300 "word"
  , NP.Seq 1300 1301
  , NP.Assign 1301 "opcode" (Num 0)
  , NP.Seq 1301 1302
  , NP.Assign 1302 "oparg" (Num 0)
  , NP.Seq 1302 1303
  , NP.Seq 1302 1299
  , NP.Var 1303 "LOOP_FOOTER"
  , NP.Seq 1303 1304
  , NP.Seq 1303 35
  , NP.Branch 1304 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1306 1551
  , NP.Var 1306 "NOP_1306"
  , NP.Var 1307 "__CLABEL_TARGET_CALL"
  , NP.Seq 1307 1308
  , NP.Assign 1308 "undefed" (Num 0)
  , NP.Seq 1308 1309
  , NP.Assign 1309 "next_instr" (Num 0)
  , NP.Seq 1309 1310
  , NP.Var 1310 "__CLABEL_PREDICTED_CALL"
  , NP.Seq 1310 1311
  , NP.Var 1311 "NOP_1311"
  , NP.Var 1312 "this_instr"
  , NP.Seq 1312 1313
  , NP.Assign 1313 "opcode" (Num 0)
  , NP.Seq 1313 1314
  , NP.Var 1314 "callable"
  , NP.Seq 1314 1315
  , NP.Var 1315 "self_or_null"
  , NP.Seq 1315 1316
  , NP.Var 1316 "args"
  , NP.Seq 1316 1317
  , NP.Var 1317 "res"
  , NP.Seq 1317 1318
  , NP.Assign 1318 "self_or_null" (Num 0)
  , NP.Seq 1318 1319
  , NP.Assign 1319 "callable" (Num 0)
  , NP.Seq 1319 1320
  , NP.Var 1320 "counter"
  , NP.Seq 1320 1321
  , NP.Branch 1321 (Eq (Num 0) (Num 1)) 1323 1326
  , NP.Assign 1323 "next_instr" (Num 0)
  , NP.Seq 1323 1324
  , NP.Assign 1324 "stack_pointer" (Num 0)
  , NP.Seq 1324 1325
  , NP.Assign 1325 "opcode" (Num 0)
  , NP.Seq 1325 1326
  , NP.Seq 1325 35
  , NP.Seq 1325 1327
  , NP.Var 1326 "NOP_1326"
  , NP.Seq 1326 1327
  , NP.Var 1327 "IF_ELSE_FOOTER"
  , NP.Assign 1328 "undefed" (Num 0)
  , NP.Seq 1328 1329
  , NP.Branch 1329 (Eq (Num 0) (Num 1)) 1330 1331
  , NP.Assign 1330 "undefed" (Num 0)
  , NP.Seq 1330 1329
  , NP.Var 1331 "LOOP_FOOTER"
  , NP.Seq 1331 1332
  , NP.Branch 1332 (Eq (Plus (Plus (Num 0) (Plus (Id "PyMethod_Type") (Num 0))) (Plus (Num 0) (Num 0))) (Num 1)) 1334 1343
  , NP.Var 1334 "callable_o"
  , NP.Seq 1334 1335
  , NP.Var 1335 "self"
  , NP.Seq 1335 1336
  , NP.Assign 1336 "self_or_null" (Num 0)
  , NP.Seq 1336 1337
  , NP.Var 1337 "method"
  , NP.Seq 1337 1338
  , NP.Var 1338 "temp"
  , NP.Seq 1338 1339
  , NP.Assign 1339 "callable" (Num 0)
  , NP.Seq 1339 1340
  , NP.Assign 1340 "undefed" (Num 0)
  , NP.Seq 1340 1341
  , NP.Assign 1341 "undefed" (Num 0)
  , NP.Seq 1341 1342
  , NP.Assign 1342 "stack_pointer" (Num 0)
  , NP.Seq 1342 1343
  , NP.Seq 1342 1344
  , NP.Var 1343 "NOP_1343"
  , NP.Seq 1343 1344
  , NP.Var 1344 "IF_ELSE_FOOTER"
  , NP.Assign 1345 "args" (Num 0)
  , NP.Seq 1345 1346
  , NP.Var 1346 "callable_o"
  , NP.Seq 1346 1347
  , NP.Var 1347 "total_args"
  , NP.Seq 1347 1348
  , NP.Var 1348 "arguments"
  , NP.Seq 1348 1349
  , NP.Branch 1349 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1351 1352
  , NP.Assign 1351 "total_args" (Num 0)
  , NP.Seq 1351 1352
  , NP.Seq 1351 1353
  , NP.Var 1352 "NOP_1352"
  , NP.Seq 1352 1353
  , NP.Var 1353 "IF_ELSE_FOOTER"
  , NP.Branch 1354 (Eq (Plus (Plus (Plus (Num 0) (Plus (Id "PyFunction_Type") (Num 0))) (Plus (Num 0) (Num 0))) (Plus (Num 0) (Id "_PyFunction_Vectorcall"))) (Num 1)) 1356 1372
  , NP.Var 1356 "code_flags"
  , NP.Seq 1356 1357
  , NP.Var 1357 "locals"
  , NP.Seq 1357 1358
  , NP.Assign 1358 "undefed" (Num 0)
  , NP.Seq 1358 1359
  , NP.Assign 1359 "undefed" (Num 0)
  , NP.Seq 1359 1360
  , NP.Var 1360 "new_frame"
  , NP.Seq 1360 1361
  , NP.Assign 1361 "stack_pointer" (Num 0)
  , NP.Seq 1361 1362
  , NP.Assign 1362 "stack_pointer" (Num 0)
  , NP.Seq 1362 1363
  , NP.Branch 1363 (Eq (Plus (Id "new_frame") (Num 0)) (Num 1)) 1365 1365
  , NP.Seq 1364 1817
  , NP.Seq 1364 1366
  , NP.Var 1365 "NOP_1365"
  , NP.Seq 1365 1366
  , NP.Var 1366 "IF_ELSE_FOOTER"
  , NP.Assign 1367 "undefed" (Num 0)
  , NP.Seq 1367 1368
  , NP.Assign 1368 "frame" (Num 0)
  , NP.Seq 1368 1369
  , NP.Seq 1368 1886
  , NP.Branch 1369 (Eq (Num 0) (Num 1)) 1370 1371
  , NP.Assign 1370 "frame" (Num 0)
  , NP.Seq 1370 1371
  , NP.Seq 1370 1886
  , NP.Seq 1370 1369
  , NP.Var 1371 "LOOP_FOOTER"
  , NP.Seq 1371 1372
  , NP.Seq 1371 1373
  , NP.Var 1372 "NOP_1372"
  , NP.Seq 1372 1373
  , NP.Var 1373 "IF_ELSE_FOOTER"
  , NP.Var 1374 "args_o_temp"
  , NP.Seq 1374 1375
  , NP.Var 1375 "args_o"
  , NP.Seq 1375 1376
  , NP.Branch 1376 (Eq (Plus (Id "args_o") (Num 0)) (Num 1)) 1378 1394
  , NP.Var 1378 "tmp"
  , NP.Seq 1378 1379
  , NP.Var 1379 "_i"
  , NP.Seq 1379 1380
  , NP.Branch 1380 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1381 1385
  , NP.Assign 1381 "tmp" (Num 0)
  , NP.Seq 1381 1382
  , NP.Assign 1382 "undefed" (Num 0)
  , NP.Seq 1382 1383
  , NP.Assign 1383 "undefed" (Num 0)
  , NP.Seq 1383 1384
  , NP.Assign 1384 "undefed" (Num 0)
  , NP.Seq 1384 1385
  , NP.Seq 1384 1380
  , NP.Var 1385 "LOOP_FOOTER"
  , NP.Seq 1385 1386
  , NP.Assign 1386 "tmp" (Num 0)
  , NP.Seq 1386 1387
  , NP.Assign 1387 "self_or_null" (Num 0)
  , NP.Seq 1387 1388
  , NP.Assign 1388 "undefed" (Num 0)
  , NP.Seq 1388 1389
  , NP.Assign 1389 "tmp" (Num 0)
  , NP.Seq 1389 1390
  , NP.Assign 1390 "callable" (Num 0)
  , NP.Seq 1390 1391
  , NP.Assign 1391 "undefed" (Num 0)
  , NP.Seq 1391 1392
  , NP.Assign 1392 "stack_pointer" (Num 0)
  , NP.Seq 1392 1393
  , NP.Assign 1393 "stack_pointer" (Num 0)
  , NP.Seq 1393 1394
  , NP.Seq 1393 1817
  , NP.Seq 1393 1395
  , NP.Var 1394 "NOP_1394"
  , NP.Seq 1394 1395
  , NP.Var 1395 "IF_ELSE_FOOTER"
  , NP.Assign 1396 "undefed" (Num 0)
  , NP.Seq 1396 1397
  , NP.Assign 1397 "undefed" (Num 0)
  , NP.Seq 1397 1398
  , NP.Var 1398 "res_o"
  , NP.Seq 1398 1399
  , NP.Assign 1399 "stack_pointer" (Num 0)
  , NP.Seq 1399 1400
  , NP.Branch 1400 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1402 1514
  , NP.Var 1402 "arg"
  , NP.Seq 1402 1403
  , NP.Branch 1403 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 1405 1406
  , NP.Assign 1405 "stack_pointer" (Num 0)
  , NP.Seq 1405 1513
  , NP.Var 1406 "err"
  , NP.Seq 1406 1407
  , NP.Assign 1407 "stack_pointer" (Num 0)
  , NP.Seq 1407 1408
  , NP.Branch 1408 (Eq (Plus (Id "err") (Num 0)) (Num 1)) 1410 1511
  , NP.Assign 1410 "undefed" (Num 0)
  , NP.Seq 1410 1411
  , NP.Assign 1411 "_tmp_old_op" (Num 0)
  , NP.Seq 1411 1412
  , NP.Branch 1412 (Eq (Plus (Id "_tmp_old_op") (Num 0)) (Num 1)) 1414 1457
  , NP.Assign 1414 "undefed" (Num 0)
  , NP.Seq 1414 1415
  , NP.Var 1415 "op"
  , NP.Seq 1415 1416
  , NP.Branch 1416 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1418 1433
  , NP.Var 1418 "tracer"
  , NP.Seq 1418 1419
  , NP.Branch 1419 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1421 1422
  , NP.Var 1421 "data"
  , NP.Seq 1421 1422
  , NP.Seq 1421 1423
  , NP.Var 1422 "NOP_1422"
  , NP.Seq 1422 1423
  , NP.Var 1423 "IF_ELSE_FOOTER"
  , NP.Branch 1424 (Eq (Num 0) (Num 1)) 1425 1431
  , NP.Var 1425 "tracer"
  , NP.Seq 1425 1426
  , NP.Branch 1426 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1428 1429
  , NP.Var 1428 "data"
  , NP.Seq 1428 1429
  , NP.Seq 1428 1430
  , NP.Var 1429 "NOP_1429"
  , NP.Seq 1429 1430
  , NP.Var 1430 "IF_ELSE_FOOTER"
  , NP.Seq 1430 1424
  , NP.Var 1431 "LOOP_FOOTER"
  , NP.Seq 1431 1432
  , NP.Var 1432 "dealloc"
  , NP.Seq 1432 1433
  , NP.Seq 1432 1434
  , NP.Var 1433 "NOP_1433"
  , NP.Seq 1433 1434
  , NP.Var 1434 "IF_ELSE_FOOTER"
  , NP.Branch 1435 (Eq (Num 0) (Num 1)) 1436 1456
  , NP.Var 1436 "op"
  , NP.Seq 1436 1437
  , NP.Branch 1437 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1439 1454
  , NP.Var 1439 "tracer"
  , NP.Seq 1439 1440
  , NP.Branch 1440 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1442 1443
  , NP.Var 1442 "data"
  , NP.Seq 1442 1443
  , NP.Seq 1442 1444
  , NP.Var 1443 "NOP_1443"
  , NP.Seq 1443 1444
  , NP.Var 1444 "IF_ELSE_FOOTER"
  , NP.Branch 1445 (Eq (Num 0) (Num 1)) 1446 1452
  , NP.Var 1446 "tracer"
  , NP.Seq 1446 1447
  , NP.Branch 1447 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1449 1450
  , NP.Var 1449 "data"
  , NP.Seq 1449 1450
  , NP.Seq 1449 1451
  , NP.Var 1450 "NOP_1450"
  , NP.Seq 1450 1451
  , NP.Var 1451 "IF_ELSE_FOOTER"
  , NP.Seq 1451 1445
  , NP.Var 1452 "LOOP_FOOTER"
  , NP.Seq 1452 1453
  , NP.Var 1453 "dealloc"
  , NP.Seq 1453 1454
  , NP.Seq 1453 1455
  , NP.Var 1454 "NOP_1454"
  , NP.Seq 1454 1455
  , NP.Var 1455 "IF_ELSE_FOOTER"
  , NP.Seq 1455 1435
  , NP.Var 1456 "LOOP_FOOTER"
  , NP.Seq 1456 1457
  , NP.Seq 1456 1458
  , NP.Var 1457 "NOP_1457"
  , NP.Seq 1457 1458
  , NP.Var 1458 "IF_ELSE_FOOTER"
  , NP.Branch 1459 (Eq (Num 0) (Num 1)) 1460 1509
  , NP.Assign 1460 "undefed" (Num 0)
  , NP.Seq 1460 1461
  , NP.Assign 1461 "_tmp_old_op" (Num 0)
  , NP.Seq 1461 1462
  , NP.Branch 1462 (Eq (Plus (Id "_tmp_old_op") (Num 0)) (Num 1)) 1464 1507
  , NP.Assign 1464 "undefed" (Num 0)
  , NP.Seq 1464 1465
  , NP.Var 1465 "op"
  , NP.Seq 1465 1466
  , NP.Branch 1466 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1468 1483
  , NP.Var 1468 "tracer"
  , NP.Seq 1468 1469
  , NP.Branch 1469 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1471 1472
  , NP.Var 1471 "data"
  , NP.Seq 1471 1472
  , NP.Seq 1471 1473
  , NP.Var 1472 "NOP_1472"
  , NP.Seq 1472 1473
  , NP.Var 1473 "IF_ELSE_FOOTER"
  , NP.Branch 1474 (Eq (Num 0) (Num 1)) 1475 1481
  , NP.Var 1475 "tracer"
  , NP.Seq 1475 1476
  , NP.Branch 1476 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1478 1479
  , NP.Var 1478 "data"
  , NP.Seq 1478 1479
  , NP.Seq 1478 1480
  , NP.Var 1479 "NOP_1479"
  , NP.Seq 1479 1480
  , NP.Var 1480 "IF_ELSE_FOOTER"
  , NP.Seq 1480 1474
  , NP.Var 1481 "LOOP_FOOTER"
  , NP.Seq 1481 1482
  , NP.Var 1482 "dealloc"
  , NP.Seq 1482 1483
  , NP.Seq 1482 1484
  , NP.Var 1483 "NOP_1483"
  , NP.Seq 1483 1484
  , NP.Var 1484 "IF_ELSE_FOOTER"
  , NP.Branch 1485 (Eq (Num 0) (Num 1)) 1486 1506
  , NP.Var 1486 "op"
  , NP.Seq 1486 1487
  , NP.Branch 1487 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1489 1504
  , NP.Var 1489 "tracer"
  , NP.Seq 1489 1490
  , NP.Branch 1490 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1492 1493
  , NP.Var 1492 "data"
  , NP.Seq 1492 1493
  , NP.Seq 1492 1494
  , NP.Var 1493 "NOP_1493"
  , NP.Seq 1493 1494
  , NP.Var 1494 "IF_ELSE_FOOTER"
  , NP.Branch 1495 (Eq (Num 0) (Num 1)) 1496 1502
  , NP.Var 1496 "tracer"
  , NP.Seq 1496 1497
  , NP.Branch 1497 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1499 1500
  , NP.Var 1499 "data"
  , NP.Seq 1499 1500
  , NP.Seq 1499 1501
  , NP.Var 1500 "NOP_1500"
  , NP.Seq 1500 1501
  , NP.Var 1501 "IF_ELSE_FOOTER"
  , NP.Seq 1501 1495
  , NP.Var 1502 "LOOP_FOOTER"
  , NP.Seq 1502 1503
  , NP.Var 1503 "dealloc"
  , NP.Seq 1503 1504
  , NP.Seq 1503 1505
  , NP.Var 1504 "NOP_1504"
  , NP.Seq 1504 1505
  , NP.Var 1505 "IF_ELSE_FOOTER"
  , NP.Seq 1505 1485
  , NP.Var 1506 "LOOP_FOOTER"
  , NP.Seq 1506 1507
  , NP.Seq 1506 1508
  , NP.Var 1507 "NOP_1507"
  , NP.Seq 1507 1508
  , NP.Var 1508 "IF_ELSE_FOOTER"
  , NP.Seq 1508 1459
  , NP.Var 1509 "LOOP_FOOTER"
  , NP.Seq 1509 1510
  , NP.Assign 1510 "stack_pointer" (Num 0)
  , NP.Seq 1510 1511
  , NP.Seq 1510 1512
  , NP.Var 1511 "NOP_1511"
  , NP.Seq 1511 1512
  , NP.Var 1512 "IF_ELSE_FOOTER"
  , NP.Var 1513 "IF_ELSE_FOOTER"
  , NP.Seq 1513 1515
  , NP.Var 1514 "NOP_1514"
  , NP.Seq 1514 1515
  , NP.Var 1515 "IF_ELSE_FOOTER"
  , NP.Var 1516 "tmp"
  , NP.Seq 1516 1517
  , NP.Var 1517 "_i"
  , NP.Seq 1517 1518
  , NP.Branch 1518 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1519 1521
  , NP.Assign 1519 "tmp" (Num 0)
  , NP.Seq 1519 1520
  , NP.Assign 1520 "undefed" (Num 0)
  , NP.Seq 1520 1521
  , NP.Seq 1520 1518
  , NP.Var 1521 "LOOP_FOOTER"
  , NP.Seq 1521 1522
  , NP.Assign 1522 "tmp" (Num 0)
  , NP.Seq 1522 1523
  , NP.Assign 1523 "self_or_null" (Num 0)
  , NP.Seq 1523 1524
  , NP.Assign 1524 "undefed" (Num 0)
  , NP.Seq 1524 1525
  , NP.Assign 1525 "tmp" (Num 0)
  , NP.Seq 1525 1526
  , NP.Assign 1526 "callable" (Num 0)
  , NP.Seq 1526 1527
  , NP.Assign 1527 "undefed" (Num 0)
  , NP.Seq 1527 1528
  , NP.Assign 1528 "stack_pointer" (Num 0)
  , NP.Seq 1528 1529
  , NP.Assign 1529 "stack_pointer" (Num 0)
  , NP.Seq 1529 1530
  , NP.Branch 1530 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 1532 1532
  , NP.Seq 1531 1817
  , NP.Seq 1531 1533
  , NP.Var 1532 "NOP_1532"
  , NP.Seq 1532 1533
  , NP.Var 1533 "IF_ELSE_FOOTER"
  , NP.Assign 1534 "res" (Num 0)
  , NP.Seq 1534 1535
  , NP.Assign 1535 "undefed" (Num 0)
  , NP.Seq 1535 1536
  , NP.Assign 1536 "stack_pointer" (Num 0)
  , NP.Seq 1536 1537
  , NP.Var 1537 "err"
  , NP.Seq 1537 1538
  , NP.Assign 1538 "stack_pointer" (Num 0)
  , NP.Seq 1538 1539
  , NP.Branch 1539 (Eq (Plus (Id "err") (Num 0)) (Num 1)) 1541 1541
  , NP.Seq 1540 1817
  , NP.Seq 1540 1542
  , NP.Var 1541 "NOP_1541"
  , NP.Seq 1541 1542
  , NP.Var 1542 "IF_ELSE_FOOTER"
  , NP.Var 1543 "word"
  , NP.Seq 1543 1544
  , NP.Assign 1544 "opcode" (Num 0)
  , NP.Seq 1544 1545
  , NP.Assign 1545 "oparg" (Num 0)
  , NP.Seq 1545 1546
  , NP.Branch 1546 (Eq (Num 0) (Num 1)) 1547 1550
  , NP.Var 1547 "word"
  , NP.Seq 1547 1548
  , NP.Assign 1548 "opcode" (Num 0)
  , NP.Seq 1548 1549
  , NP.Assign 1549 "oparg" (Num 0)
  , NP.Seq 1549 1550
  , NP.Seq 1549 1546
  , NP.Var 1550 "LOOP_FOOTER"
  , NP.Seq 1550 1551
  , NP.Seq 1550 35
  , NP.Branch 1551 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1553 1637
  , NP.Var 1553 "NOP_1553"
  , NP.Var 1554 "__CLABEL_TARGET_CALL_ALLOC_AND_ENTER_INIT"
  , NP.Seq 1554 1555
  , NP.Var 1555 "this_instr"
  , NP.Seq 1555 1556
  , NP.Assign 1556 "undefed" (Num 0)
  , NP.Seq 1556 1557
  , NP.Assign 1557 "next_instr" (Num 0)
  , NP.Seq 1557 1558
  , NP.Var 1558 "callable"
  , NP.Seq 1558 1559
  , NP.Var 1559 "self_or_null"
  , NP.Seq 1559 1560
  , NP.Var 1560 "init"
  , NP.Seq 1560 1561
  , NP.Var 1561 "self"
  , NP.Seq 1561 1562
  , NP.Var 1562 "args"
  , NP.Seq 1562 1563
  , NP.Var 1563 "init_frame"
  , NP.Seq 1563 1564
  , NP.Var 1564 "new_frame"
  , NP.Seq 1564 1565
  , NP.Branch 1565 (Eq (Num 0) (Num 1)) 1567 1567
  , NP.Seq 1566 1310
  , NP.Seq 1566 1568
  , NP.Var 1567 "NOP_1567"
  , NP.Seq 1567 1568
  , NP.Var 1568 "IF_ELSE_FOOTER"
  , NP.Assign 1569 "self_or_null" (Num 0)
  , NP.Seq 1569 1570
  , NP.Assign 1570 "callable" (Num 0)
  , NP.Seq 1570 1571
  , NP.Var 1571 "type_version"
  , NP.Seq 1571 1572
  , NP.Var 1572 "callable_o"
  , NP.Seq 1572 1573
  , NP.Branch 1573 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1575 1575
  , NP.Seq 1574 1310
  , NP.Seq 1574 1576
  , NP.Var 1575 "NOP_1575"
  , NP.Seq 1575 1576
  , NP.Var 1576 "IF_ELSE_FOOTER"
  , NP.Branch 1577 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1579 1579
  , NP.Seq 1578 1310
  , NP.Seq 1578 1580
  , NP.Var 1579 "NOP_1579"
  , NP.Seq 1579 1580
  , NP.Var 1580 "IF_ELSE_FOOTER"
  , NP.Var 1581 "tp"
  , NP.Seq 1581 1582
  , NP.Branch 1582 (Eq (Plus (Num 0) (Id "type_version")) (Num 1)) 1584 1584
  , NP.Seq 1583 1310
  , NP.Seq 1583 1585
  , NP.Var 1584 "NOP_1584"
  , NP.Seq 1584 1585
  , NP.Var 1585 "IF_ELSE_FOOTER"
  , NP.Var 1586 "cls"
  , NP.Seq 1586 1587
  , NP.Var 1587 "init_func"
  , NP.Seq 1587 1588
  , NP.Var 1588 "code"
  , NP.Seq 1588 1589
  , NP.Branch 1589 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1591 1591
  , NP.Seq 1590 1310
  , NP.Seq 1590 1592
  , NP.Var 1591 "NOP_1591"
  , NP.Seq 1591 1592
  , NP.Var 1592 "IF_ELSE_FOOTER"
  , NP.Var 1593 "self_o"
  , NP.Seq 1593 1594
  , NP.Assign 1594 "stack_pointer" (Num 0)
  , NP.Seq 1594 1595
  , NP.Branch 1595 (Eq (Plus (Id "self_o") (Num 0)) (Num 1)) 1597 1597
  , NP.Seq 1596 1817
  , NP.Seq 1596 1598
  , NP.Var 1597 "NOP_1597"
  , NP.Seq 1597 1598
  , NP.Var 1598 "IF_ELSE_FOOTER"
  , NP.Assign 1599 "self_or_null" (Num 0)
  , NP.Seq 1599 1600
  , NP.Var 1600 "temp"
  , NP.Seq 1600 1601
  , NP.Assign 1601 "callable" (Num 0)
  , NP.Seq 1601 1602
  , NP.Assign 1602 "undefed" (Num 0)
  , NP.Seq 1602 1603
  , NP.Assign 1603 "undefed" (Num 0)
  , NP.Seq 1603 1604
  , NP.Assign 1604 "stack_pointer" (Num 0)
  , NP.Seq 1604 1605
  , NP.Assign 1605 "args" (Num 0)
  , NP.Seq 1605 1606
  , NP.Assign 1606 "self" (Num 0)
  , NP.Seq 1606 1607
  , NP.Assign 1607 "init" (Num 0)
  , NP.Seq 1607 1608
  , NP.Var 1608 "shim"
  , NP.Seq 1608 1609
  , NP.Assign 1609 "stack_pointer" (Num 0)
  , NP.Seq 1609 1610
  , NP.Assign 1610 "undefed" (Num 0)
  , NP.Seq 1610 1611
  , NP.Var 1611 "temp"
  , NP.Seq 1611 1612
  , NP.Assign 1612 "stack_pointer" (Num 0)
  , NP.Seq 1612 1613
  , NP.Assign 1613 "stack_pointer" (Num 0)
  , NP.Seq 1613 1614
  , NP.Branch 1614 (Eq (Plus (Id "temp") (Num 0)) (Num 1)) 1616 1617
  , NP.Assign 1616 "stack_pointer" (Num 0)
  , NP.Seq 1616 1617
  , NP.Seq 1616 1817
  , NP.Seq 1616 1618
  , NP.Var 1617 "NOP_1617"
  , NP.Seq 1617 1618
  , NP.Var 1618 "IF_ELSE_FOOTER"
  , NP.Assign 1619 "undefed" (Num 0)
  , NP.Seq 1619 1620
  , NP.Assign 1620 "init_frame" (Num 0)
  , NP.Seq 1620 1621
  , NP.Assign 1621 "new_frame" (Num 0)
  , NP.Seq 1621 1622
  , NP.Var 1622 "temp"
  , NP.Seq 1622 1623
  , NP.Assign 1623 "frame" (Num 0)
  , NP.Seq 1623 1624
  , NP.Assign 1624 "stack_pointer" (Num 0)
  , NP.Seq 1624 1625
  , NP.Assign 1625 "next_instr" (Num 0)
  , NP.Seq 1625 1626
  , NP.Branch 1626 (Eq (Num 0) (Num 1)) 1627 1628
  , NP.Assign 1627 "next_instr" (Num 0)
  , NP.Seq 1627 1626
  , NP.Var 1628 "LOOP_FOOTER"
  , NP.Seq 1628 1629
  , NP.Var 1629 "word"
  , NP.Seq 1629 1630
  , NP.Assign 1630 "opcode" (Num 0)
  , NP.Seq 1630 1631
  , NP.Assign 1631 "oparg" (Num 0)
  , NP.Seq 1631 1632
  , NP.Branch 1632 (Eq (Num 0) (Num 1)) 1633 1636
  , NP.Var 1633 "word"
  , NP.Seq 1633 1634
  , NP.Assign 1634 "opcode" (Num 0)
  , NP.Seq 1634 1635
  , NP.Assign 1635 "oparg" (Num 0)
  , NP.Seq 1635 1636
  , NP.Seq 1635 1632
  , NP.Var 1636 "LOOP_FOOTER"
  , NP.Seq 1636 1637
  , NP.Seq 1636 35
  , NP.Branch 1637 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1639 1727
  , NP.Var 1639 "NOP_1639"
  , NP.Var 1640 "__CLABEL_TARGET_CALL_BOUND_METHOD_EXACT_ARGS"
  , NP.Seq 1640 1641
  , NP.Var 1641 "this_instr"
  , NP.Seq 1641 1642
  , NP.Assign 1642 "undefed" (Num 0)
  , NP.Seq 1642 1643
  , NP.Assign 1643 "next_instr" (Num 0)
  , NP.Seq 1643 1644
  , NP.Var 1644 "callable"
  , NP.Seq 1644 1645
  , NP.Var 1645 "null"
  , NP.Seq 1645 1646
  , NP.Var 1646 "self_or_null"
  , NP.Seq 1646 1647
  , NP.Var 1647 "args"
  , NP.Seq 1647 1648
  , NP.Var 1648 "new_frame"
  , NP.Seq 1648 1649
  , NP.Branch 1649 (Eq (Num 0) (Num 1)) 1651 1651
  , NP.Seq 1650 1310
  , NP.Seq 1650 1652
  , NP.Var 1651 "NOP_1651"
  , NP.Seq 1651 1652
  , NP.Var 1652 "IF_ELSE_FOOTER"
  , NP.Assign 1653 "null" (Num 0)
  , NP.Seq 1653 1654
  , NP.Assign 1654 "callable" (Num 0)
  , NP.Seq 1654 1655
  , NP.Branch 1655 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1657 1657
  , NP.Seq 1656 1310
  , NP.Seq 1656 1658
  , NP.Var 1657 "NOP_1657"
  , NP.Seq 1657 1658
  , NP.Var 1658 "IF_ELSE_FOOTER"
  , NP.Branch 1659 (Eq (Plus (Num 0) (Plus (Id "PyMethod_Type") (Num 0))) (Num 1)) 1661 1661
  , NP.Seq 1660 1310
  , NP.Seq 1660 1662
  , NP.Var 1661 "NOP_1661"
  , NP.Seq 1661 1662
  , NP.Var 1662 "IF_ELSE_FOOTER"
  , NP.Assign 1663 "self_or_null" (Num 0)
  , NP.Seq 1663 1664
  , NP.Var 1664 "callable_o"
  , NP.Seq 1664 1665
  , NP.Assign 1665 "self_or_null" (Num 0)
  , NP.Seq 1665 1666
  , NP.Var 1666 "temp"
  , NP.Seq 1666 1667
  , NP.Assign 1667 "callable" (Num 0)
  , NP.Seq 1667 1668
  , NP.Assign 1668 "undefed" (Num 0)
  , NP.Seq 1668 1669
  , NP.Assign 1669 "undefed" (Num 0)
  , NP.Seq 1669 1670
  , NP.Assign 1670 "stack_pointer" (Num 0)
  , NP.Seq 1670 1671
  , NP.Var 1671 "func_version"
  , NP.Seq 1671 1672
  , NP.Var 1672 "callable_o"
  , NP.Seq 1672 1673
  , NP.Branch 1673 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFunction_Type") (Num 0))) (Num 0)) (Num 1)) 1675 1675
  , NP.Seq 1674 1310
  , NP.Seq 1674 1676
  , NP.Var 1675 "NOP_1675"
  , NP.Seq 1675 1676
  , NP.Var 1676 "IF_ELSE_FOOTER"
  , NP.Var 1677 "func"
  , NP.Seq 1677 1678
  , NP.Branch 1678 (Eq (Plus (Num 0) (Id "func_version")) (Num 1)) 1680 1680
  , NP.Seq 1679 1310
  , NP.Seq 1679 1681
  , NP.Var 1680 "NOP_1680"
  , NP.Seq 1680 1681
  , NP.Var 1681 "IF_ELSE_FOOTER"
  , NP.Var 1682 "callable_o"
  , NP.Seq 1682 1683
  , NP.Var 1683 "func"
  , NP.Seq 1683 1684
  , NP.Var 1684 "code"
  , NP.Seq 1684 1685
  , NP.Branch 1685 (Eq (Plus (Num 0) (Plus (Id "oparg") (Plus (Plus (Num 0) (Num 0)) (Num 0)))) (Num 1)) 1687 1687
  , NP.Seq 1686 1310
  , NP.Seq 1686 1688
  , NP.Var 1687 "NOP_1687"
  , NP.Seq 1687 1688
  , NP.Var 1688 "IF_ELSE_FOOTER"
  , NP.Var 1689 "callable_o"
  , NP.Seq 1689 1690
  , NP.Var 1690 "func"
  , NP.Seq 1690 1691
  , NP.Var 1691 "code"
  , NP.Seq 1691 1692
  , NP.Branch 1692 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1694 1694
  , NP.Seq 1693 1310
  , NP.Seq 1693 1695
  , NP.Var 1694 "NOP_1694"
  , NP.Seq 1694 1695
  , NP.Var 1695 "IF_ELSE_FOOTER"
  , NP.Branch 1696 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1698 1698
  , NP.Seq 1697 1310
  , NP.Seq 1697 1699
  , NP.Var 1698 "NOP_1698"
  , NP.Seq 1698 1699
  , NP.Var 1699 "IF_ELSE_FOOTER"
  , NP.Assign 1700 "args" (Num 0)
  , NP.Seq 1700 1701
  , NP.Var 1701 "has_self"
  , NP.Seq 1701 1702
  , NP.Var 1702 "pushed_frame"
  , NP.Seq 1702 1703
  , NP.Var 1703 "first_non_self_local"
  , NP.Seq 1703 1704
  , NP.Assign 1704 "undefed" (Num 0)
  , NP.Seq 1704 1705
  , NP.Var 1705 "i"
  , NP.Seq 1705 1706
  , NP.Branch 1706 (Eq (Plus (Id "i") (Id "oparg")) (Num 1)) 1707 1708
  , NP.Assign 1707 "undefed" (Num 0)
  , NP.Seq 1707 1708
  , NP.Seq 1707 1706
  , NP.Var 1708 "LOOP_FOOTER"
  , NP.Seq 1708 1709
  , NP.Assign 1709 "new_frame" (Num 0)
  , NP.Seq 1709 1710
  , NP.Assign 1710 "undefed" (Num 0)
  , NP.Seq 1710 1711
  , NP.Var 1711 "temp"
  , NP.Seq 1711 1712
  , NP.Assign 1712 "stack_pointer" (Num 0)
  , NP.Seq 1712 1713
  , NP.Assign 1713 "frame" (Num 0)
  , NP.Seq 1713 1714
  , NP.Assign 1714 "stack_pointer" (Num 0)
  , NP.Seq 1714 1715
  , NP.Assign 1715 "next_instr" (Num 0)
  , NP.Seq 1715 1716
  , NP.Branch 1716 (Eq (Num 0) (Num 1)) 1717 1718
  , NP.Assign 1717 "next_instr" (Num 0)
  , NP.Seq 1717 1716
  , NP.Var 1718 "LOOP_FOOTER"
  , NP.Seq 1718 1719
  , NP.Var 1719 "word"
  , NP.Seq 1719 1720
  , NP.Assign 1720 "opcode" (Num 0)
  , NP.Seq 1720 1721
  , NP.Assign 1721 "oparg" (Num 0)
  , NP.Seq 1721 1722
  , NP.Branch 1722 (Eq (Num 0) (Num 1)) 1723 1726
  , NP.Var 1723 "word"
  , NP.Seq 1723 1724
  , NP.Assign 1724 "opcode" (Num 0)
  , NP.Seq 1724 1725
  , NP.Assign 1725 "oparg" (Num 0)
  , NP.Seq 1725 1726
  , NP.Seq 1725 1722
  , NP.Var 1726 "LOOP_FOOTER"
  , NP.Seq 1726 1727
  , NP.Seq 1726 35
  , NP.Branch 1727 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1729 1810
  , NP.Var 1729 "NOP_1729"
  , NP.Var 1730 "__CLABEL_TARGET_CALL_BOUND_METHOD_GENERAL"
  , NP.Seq 1730 1731
  , NP.Var 1731 "this_instr"
  , NP.Seq 1731 1732
  , NP.Assign 1732 "undefed" (Num 0)
  , NP.Seq 1732 1733
  , NP.Assign 1733 "next_instr" (Num 0)
  , NP.Seq 1733 1734
  , NP.Var 1734 "callable"
  , NP.Seq 1734 1735
  , NP.Var 1735 "null"
  , NP.Seq 1735 1736
  , NP.Var 1736 "self_or_null"
  , NP.Seq 1736 1737
  , NP.Var 1737 "args"
  , NP.Seq 1737 1738
  , NP.Var 1738 "new_frame"
  , NP.Seq 1738 1739
  , NP.Branch 1739 (Eq (Num 0) (Num 1)) 1741 1741
  , NP.Seq 1740 1310
  , NP.Seq 1740 1742
  , NP.Var 1741 "NOP_1741"
  , NP.Seq 1741 1742
  , NP.Var 1742 "IF_ELSE_FOOTER"
  , NP.Assign 1743 "null" (Num 0)
  , NP.Seq 1743 1744
  , NP.Assign 1744 "callable" (Num 0)
  , NP.Seq 1744 1745
  , NP.Var 1745 "func_version"
  , NP.Seq 1745 1746
  , NP.Var 1746 "callable_o"
  , NP.Seq 1746 1747
  , NP.Branch 1747 (Eq (Plus (Num 0) (Plus (Id "PyMethod_Type") (Num 0))) (Num 1)) 1749 1749
  , NP.Seq 1748 1310
  , NP.Seq 1748 1750
  , NP.Var 1749 "NOP_1749"
  , NP.Seq 1749 1750
  , NP.Var 1750 "IF_ELSE_FOOTER"
  , NP.Var 1751 "func"
  , NP.Seq 1751 1752
  , NP.Branch 1752 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFunction_Type") (Num 0))) (Num 0)) (Num 1)) 1754 1754
  , NP.Seq 1753 1310
  , NP.Seq 1753 1755
  , NP.Var 1754 "NOP_1754"
  , NP.Seq 1754 1755
  , NP.Var 1755 "IF_ELSE_FOOTER"
  , NP.Branch 1756 (Eq (Plus (Num 0) (Id "func_version")) (Num 1)) 1758 1758
  , NP.Seq 1757 1310
  , NP.Seq 1757 1759
  , NP.Var 1758 "NOP_1758"
  , NP.Seq 1758 1759
  , NP.Var 1759 "IF_ELSE_FOOTER"
  , NP.Branch 1760 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1762 1762
  , NP.Seq 1761 1310
  , NP.Seq 1761 1763
  , NP.Var 1762 "NOP_1762"
  , NP.Seq 1762 1763
  , NP.Var 1763 "IF_ELSE_FOOTER"
  , NP.Assign 1764 "self_or_null" (Num 0)
  , NP.Seq 1764 1765
  , NP.Var 1765 "callable_o"
  , NP.Seq 1765 1766
  , NP.Assign 1766 "self_or_null" (Num 0)
  , NP.Seq 1766 1767
  , NP.Var 1767 "temp"
  , NP.Seq 1767 1768
  , NP.Assign 1768 "callable" (Num 0)
  , NP.Seq 1768 1769
  , NP.Assign 1769 "undefed" (Num 0)
  , NP.Seq 1769 1770
  , NP.Assign 1770 "undefed" (Num 0)
  , NP.Seq 1770 1771
  , NP.Assign 1771 "stack_pointer" (Num 0)
  , NP.Seq 1771 1772
  , NP.Branch 1772 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1774 1774
  , NP.Seq 1773 1310
  , NP.Seq 1773 1775
  , NP.Var 1774 "NOP_1774"
  , NP.Seq 1774 1775
  , NP.Var 1775 "IF_ELSE_FOOTER"
  , NP.Assign 1776 "args" (Num 0)
  , NP.Seq 1776 1777
  , NP.Var 1777 "callable_o"
  , NP.Seq 1777 1778
  , NP.Var 1778 "total_args"
  , NP.Seq 1778 1779
  , NP.Branch 1779 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1781 1782
  , NP.Assign 1781 "total_args" (Num 0)
  , NP.Seq 1781 1782
  , NP.Seq 1781 1783
  , NP.Var 1782 "NOP_1782"
  , NP.Seq 1782 1783
  , NP.Var 1783 "IF_ELSE_FOOTER"
  , NP.Var 1784 "code_flags"
  , NP.Seq 1784 1785
  , NP.Var 1785 "locals"
  , NP.Seq 1785 1786
  , NP.Var 1786 "temp"
  , NP.Seq 1786 1787
  , NP.Assign 1787 "stack_pointer" (Num 0)
  , NP.Seq 1787 1788
  , NP.Assign 1788 "stack_pointer" (Num 0)
  , NP.Seq 1788 1789
  , NP.Branch 1789 (Eq (Plus (Id "temp") (Num 0)) (Num 1)) 1791 1791
  , NP.Seq 1790 1817
  , NP.Seq 1790 1792
  , NP.Var 1791 "NOP_1791"
  , NP.Seq 1791 1792
  , NP.Var 1792 "IF_ELSE_FOOTER"
  , NP.Assign 1793 "new_frame" (Num 0)
  , NP.Seq 1793 1794
  , NP.Assign 1794 "undefed" (Num 0)
  , NP.Seq 1794 1795
  , NP.Var 1795 "temp"
  , NP.Seq 1795 1796
  , NP.Assign 1796 "frame" (Num 0)
  , NP.Seq 1796 1797
  , NP.Assign 1797 "stack_pointer" (Num 0)
  , NP.Seq 1797 1798
  , NP.Assign 1798 "next_instr" (Num 0)
  , NP.Seq 1798 1799
  , NP.Branch 1799 (Eq (Num 0) (Num 1)) 1800 1801
  , NP.Assign 1800 "next_instr" (Num 0)
  , NP.Seq 1800 1799
  , NP.Var 1801 "LOOP_FOOTER"
  , NP.Seq 1801 1802
  , NP.Var 1802 "word"
  , NP.Seq 1802 1803
  , NP.Assign 1803 "opcode" (Num 0)
  , NP.Seq 1803 1804
  , NP.Assign 1804 "oparg" (Num 0)
  , NP.Seq 1804 1805
  , NP.Branch 1805 (Eq (Num 0) (Num 1)) 1806 1809
  , NP.Var 1806 "word"
  , NP.Seq 1806 1807
  , NP.Assign 1807 "opcode" (Num 0)
  , NP.Seq 1807 1808
  , NP.Assign 1808 "oparg" (Num 0)
  , NP.Seq 1808 1809
  , NP.Seq 1808 1805
  , NP.Var 1809 "LOOP_FOOTER"
  , NP.Seq 1809 1810
  , NP.Seq 1809 35
  , NP.Var 1810 "NOP_1810"
  , NP.Seq 1810 1811
  , NP.Var 1811 "__CLABEL_CODEGEN_SWITCH_EXIT_0"
  , NP.Seq 1811 1812
  , NP.Var 1812 "NOP_1812"
  , NP.Var 1813 "__CLABEL_pop_2_error"
  , NP.Seq 1813 1814
  , NP.Assign 1814 "stack_pointer" (Num 0)
  , NP.Seq 1814 1815
  , NP.Seq 1814 1817
  , NP.Var 1815 "__CLABEL_pop_1_error"
  , NP.Seq 1815 1816
  , NP.Assign 1816 "stack_pointer" (Num 0)
  , NP.Seq 1816 1817
  , NP.Seq 1816 1817
  , NP.Var 1817 "__CLABEL_error"
  , NP.Seq 1817 1818
  , NP.Branch 1818 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1820 1821
  , NP.Assign 1820 "stack_pointer" (Num 0)
  , NP.Seq 1820 1822
  , NP.Var 1821 "NOP_1821"
  , NP.Seq 1821 1822
  , NP.Var 1822 "IF_ELSE_FOOTER"
  , NP.Branch 1823 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1825 1832
  , NP.Var 1825 "f"
  , NP.Seq 1825 1826
  , NP.Assign 1826 "stack_pointer" (Num 0)
  , NP.Seq 1826 1827
  , NP.Branch 1827 (Eq (Plus (Id "f") (Num 0)) (Num 1)) 1829 1830
  , NP.Assign 1829 "stack_pointer" (Num 0)
  , NP.Seq 1829 1831
  , NP.Var 1830 "NOP_1830"
  , NP.Seq 1830 1831
  , NP.Var 1831 "IF_ELSE_FOOTER"
  , NP.Seq 1831 1833
  , NP.Var 1832 "NOP_1832"
  , NP.Seq 1832 1833
  , NP.Var 1833 "IF_ELSE_FOOTER"
  , NP.Seq 1833 1834
  , NP.Var 1834 "__CLABEL_exception_unwind"
  , NP.Seq 1834 1835
  , NP.Var 1835 "offset"
  , NP.Seq 1835 1836
  , NP.Var 1836 "level"
  , NP.Seq 1836 1837
  , NP.Var 1837 "handler"
  , NP.Seq 1837 1838
  , NP.Var 1838 "lasti"
  , NP.Seq 1838 1839
  , NP.Var 1839 "handled"
  , NP.Seq 1839 1840
  , NP.Branch 1840 (Eq (Plus (Id "handled") (Num 0)) (Num 1)) 1842 1846
  , NP.Var 1842 "stackbase"
  , NP.Seq 1842 1843
  , NP.Branch 1843 (Eq (Plus (Num 0) (Id "stackbase")) (Num 1)) 1844 1845
  , NP.Var 1844 "ref"
  , NP.Seq 1844 1845
  , NP.Seq 1844 1843
  , NP.Var 1845 "LOOP_FOOTER"
  , NP.Seq 1845 1846
  , NP.Seq 1845 1874
  , NP.Seq 1845 1847
  , NP.Var 1846 "NOP_1846"
  , NP.Seq 1846 1847
  , NP.Var 1847 "IF_ELSE_FOOTER"
  , NP.Var 1848 "new_top"
  , NP.Seq 1848 1849
  , NP.Branch 1849 (Eq (Plus (Num 0) (Id "new_top")) (Num 1)) 1850 1851
  , NP.Var 1850 "ref"
  , NP.Seq 1850 1851
  , NP.Seq 1850 1849
  , NP.Var 1851 "LOOP_FOOTER"
  , NP.Seq 1851 1852
  , NP.Branch 1852 (Eq (Id "lasti") (Num 1)) 1854 1856
  , NP.Var 1854 "frame_lasti"
  , NP.Seq 1854 1855
  , NP.Var 1855 "lasti"
  , NP.Seq 1855 1856
  , NP.Seq 1855 1857
  , NP.Var 1856 "NOP_1856"
  , NP.Seq 1856 1857
  , NP.Var 1857 "IF_ELSE_FOOTER"
  , NP.Var 1858 "exc"
  , NP.Seq 1858 1859
  , NP.Assign 1859 "next_instr" (Num 0)
  , NP.Seq 1859 1860
  , NP.Var 1860 "err"
  , NP.Seq 1860 1861
  , NP.Branch 1861 (Eq (Plus (Id "err") (Num 0)) (Num 1)) 1863 1863
  , NP.Seq 1862 1834
  , NP.Seq 1862 1864
  , NP.Var 1863 "NOP_1863"
  , NP.Seq 1863 1864
  , NP.Var 1864 "IF_ELSE_FOOTER"
  , NP.Assign 1865 "stack_pointer" (Num 0)
  , NP.Seq 1865 1866
  , NP.Var 1866 "word"
  , NP.Seq 1866 1867
  , NP.Assign 1867 "opcode" (Num 0)
  , NP.Seq 1867 1868
  , NP.Assign 1868 "oparg" (Num 0)
  , NP.Seq 1868 1869
  , NP.Branch 1869 (Eq (Num 0) (Num 1)) 1870 1873
  , NP.Var 1870 "word"
  , NP.Seq 1870 1871
  , NP.Assign 1871 "opcode" (Num 0)
  , NP.Seq 1871 1872
  , NP.Assign 1872 "oparg" (Num 0)
  , NP.Seq 1872 1873
  , NP.Seq 1872 1869
  , NP.Var 1873 "LOOP_FOOTER"
  , NP.Seq 1873 1874
  , NP.Seq 1873 35
  , NP.Var 1874 "__CLABEL_exit_unwind"
  , NP.Seq 1874 1875
  , NP.Var 1875 "dying"
  , NP.Seq 1875 1876
  , NP.Assign 1876 "frame" (Num 0)
  , NP.Seq 1876 1877
  , NP.Assign 1877 "undefed" (Num 0)
  , NP.Seq 1877 1878
  , NP.Branch 1878 (Eq (Plus (Num 0) (Id "FRAME_OWNED_BY_INTERPRETER")) (Num 1)) 1880 1882
  , NP.Assign 1880 "undefed" (Num 0)
  , NP.Seq 1880 1881
  , NP.Assign 1881 "return" (Num 0)
  , NP.Seq 1881 1882
  , NP.Seq 1881 1883
  , NP.Var 1882 "NOP_1882"
  , NP.Seq 1882 1883
  , NP.Var 1883 "IF_ELSE_FOOTER"
  , NP.Assign 1884 "next_instr" (Num 0)
  , NP.Seq 1884 1885
  , NP.Assign 1885 "stack_pointer" (Num 0)
  , NP.Seq 1885 1886
  , NP.Seq 1885 1817
  , NP.Var 1886 "__CLABEL_start_frame"
  , NP.Seq 1886 1887
  , NP.Var 1887 "too_deep"
  , NP.Seq 1887 1888
  , NP.Branch 1888 (Eq (Id "too_deep") (Num 1)) 1890 1890
  , NP.Seq 1889 1874
  , NP.Seq 1889 1891
  , NP.Var 1890 "NOP_1890"
  , NP.Seq 1890 1891
  , NP.Var 1891 "IF_ELSE_FOOTER"
  , NP.Assign 1892 "next_instr" (Num 0)
  , NP.Seq 1892 1893
  , NP.Assign 1893 "stack_pointer" (Num 0)
  , NP.Seq 1893 1894
  , NP.Var 1894 "word"
  , NP.Seq 1894 1895
  , NP.Assign 1895 "opcode" (Num 0)
  , NP.Seq 1895 1896
  , NP.Assign 1896 "oparg" (Num 0)
  , NP.Seq 1896 1897
  , NP.Branch 1897 (Eq (Num 0) (Num 1)) 1898 1901
  , NP.Var 1898 "word"
  , NP.Seq 1898 1899
  , NP.Assign 1899 "opcode" (Num 0)
  , NP.Seq 1899 1900
  , NP.Assign 1900 "oparg" (Num 0)
  , NP.Seq 1900 1901
  , NP.Seq 1900 1897
  , NP.Var 1901 "LOOP_FOOTER"
  , NP.Seq 1901 1902
  , NP.Seq 1901 35
  , NP.Var 1902 "__CLABEL_early_exit"
  , NP.Seq 1902 1903
  , NP.Var 1903 "NOP_1903"
  , NP.Var 1904 "dying"
  , NP.Seq 1904 1905
  , NP.Assign 1905 "frame" (Num 0)
  , NP.Seq 1905 1906
  , NP.Assign 1906 "undefed" (Num 0)
  , NP.Seq 1906 1907
  , NP.Assign 1907 "undefed" (Num 0)
  , NP.Seq 1907 1908
  , NP.Assign 1908 "return" (Num 0)
  , NP.Seq 1908 1909
  , NP.Seq 1908 1909
  , NP.Var 1909 "PROG_END"
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
  , WP.Branch 7 (Eq (Num 0) (Num 1)) 9 10
  , WP.Assign 9 "return" (Num 0)
  , WP.Seq 9 11
  , WP.Var 10 "NOP_10"
  , WP.Seq 10 11
  , WP.Var 11 "IF_ELSE_FOOTER"
  , WP.Var 12 "next_instr"
  , WP.Seq 12 13
  , WP.Var 13 "stack_pointer"
  , WP.Seq 13 14
  , WP.Assign 14 "undefed" (Num 0)
  , WP.Seq 14 15
  , WP.Assign 15 "undefed" (Num 0)
  , WP.Seq 15 16
  , WP.Assign 16 "undefed" (Num 0)
  , WP.Seq 16 17
  , WP.Assign 17 "undefed" (Num 0)
  , WP.Seq 17 18
  , WP.Assign 18 "undefed" (Num 0)
  , WP.Seq 18 19
  , WP.Assign 19 "undefed" (Num 0)
  , WP.Seq 19 20
  , WP.Assign 20 "undefed" (Num 0)
  , WP.Seq 20 21
  , WP.Assign 21 "undefed" (Num 0)
  , WP.Seq 21 22
  , WP.Assign 22 "undefed" (Num 0)
  , WP.Seq 22 23
  , WP.Assign 23 "undefed" (Num 0)
  , WP.Seq 23 24
  , WP.Assign 24 "undefed" (Num 0)
  , WP.Seq 24 25
  , WP.Branch 25 (Eq (Id "throwflag") (Num 1)) 27 33
  , WP.Branch 27 (Eq (Num 0) (Num 1)) 29 29
  , WP.Seq 28 1902
  , WP.Seq 28 30
  , WP.Var 29 "NOP_29"
  , WP.Seq 29 30
  , WP.Var 30 "IF_ELSE_FOOTER"
  , WP.Assign 31 "next_instr" (Num 0)
  , WP.Seq 31 32
  , WP.Assign 32 "stack_pointer" (Num 0)
  , WP.Seq 32 33
  , WP.Seq 32 1817
  , WP.Seq 32 34
  , WP.Var 33 "NOP_33"
  , WP.Seq 33 34
  , WP.Var 34 "IF_ELSE_FOOTER"
  , WP.Seq 34 1886
  , WP.Var 35 "__CLABEL_dispatch_opcode"
  , WP.Seq 35 36
  , WP.Branch 36 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 38 87
  , WP.Var 38 "NOP_38"
  , WP.Var 39 "__CLABEL_TARGET_BINARY_OP"
  , WP.Seq 39 40
  , WP.Assign 40 "undefed" (Num 0)
  , WP.Seq 40 41
  , WP.Assign 41 "next_instr" (Num 0)
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
  , WP.Assign 48 "rhs" (Num 0)
  , WP.Seq 48 49
  , WP.Assign 49 "lhs" (Num 0)
  , WP.Seq 49 50
  , WP.Var 50 "counter"
  , WP.Seq 50 51
  , WP.Branch 51 (Eq (Num 0) (Num 1)) 53 56
  , WP.Assign 53 "next_instr" (Num 0)
  , WP.Seq 53 54
  , WP.Assign 54 "stack_pointer" (Num 0)
  , WP.Seq 54 55
  , WP.Assign 55 "opcode" (Num 0)
  , WP.Seq 55 56
  , WP.Seq 55 35
  , WP.Seq 55 57
  , WP.Var 56 "NOP_56"
  , WP.Seq 56 57
  , WP.Var 57 "IF_ELSE_FOOTER"
  , WP.Assign 58 "undefed" (Num 0)
  , WP.Seq 58 59
  , WP.Branch 59 (Eq (Num 0) (Num 1)) 60 61
  , WP.Assign 60 "undefed" (Num 0)
  , WP.Seq 60 59
  , WP.Var 61 "LOOP_FOOTER"
  , WP.Seq 61 62
  , WP.Var 62 "lhs_o"
  , WP.Seq 62 63
  , WP.Var 63 "rhs_o"
  , WP.Seq 63 64
  , WP.Var 64 "res_o"
  , WP.Seq 64 65
  , WP.Assign 65 "stack_pointer" (Num 0)
  , WP.Seq 65 66
  , WP.Branch 66 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 68 68
  , WP.Seq 67 1817
  , WP.Seq 67 69
  , WP.Var 68 "NOP_68"
  , WP.Seq 68 69
  , WP.Var 69 "IF_ELSE_FOOTER"
  , WP.Assign 70 "res" (Num 0)
  , WP.Seq 70 71
  , WP.Var 71 "tmp"
  , WP.Seq 71 72
  , WP.Assign 72 "lhs" (Num 0)
  , WP.Seq 72 73
  , WP.Assign 73 "undefed" (Num 0)
  , WP.Seq 73 74
  , WP.Assign 74 "tmp" (Num 0)
  , WP.Seq 74 75
  , WP.Assign 75 "rhs" (Num 0)
  , WP.Seq 75 76
  , WP.Assign 76 "undefed" (Num 0)
  , WP.Seq 76 77
  , WP.Assign 77 "stack_pointer" (Num 0)
  , WP.Seq 77 78
  , WP.Assign 78 "stack_pointer" (Num 0)
  , WP.Seq 78 79
  , WP.Var 79 "word"
  , WP.Seq 79 80
  , WP.Assign 80 "opcode" (Num 0)
  , WP.Seq 80 81
  , WP.Assign 81 "oparg" (Num 0)
  , WP.Seq 81 82
  , WP.Branch 82 (Eq (Num 0) (Num 1)) 83 86
  , WP.Var 83 "word"
  , WP.Seq 83 84
  , WP.Assign 84 "opcode" (Num 0)
  , WP.Seq 84 85
  , WP.Assign 85 "oparg" (Num 0)
  , WP.Seq 85 86
  , WP.Seq 85 82
  , WP.Var 86 "LOOP_FOOTER"
  , WP.Seq 86 87
  , WP.Seq 86 35
  , WP.Branch 87 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 89 129
  , WP.Var 89 "NOP_89"
  , WP.Var 90 "__CLABEL_TARGET_BINARY_OP_ADD_FLOAT"
  , WP.Seq 90 91
  , WP.Var 91 "this_instr"
  , WP.Seq 91 92
  , WP.Assign 92 "undefed" (Num 0)
  , WP.Seq 92 93
  , WP.Assign 93 "next_instr" (Num 0)
  , WP.Seq 93 94
  , WP.Var 94 "value"
  , WP.Seq 94 95
  , WP.Var 95 "left"
  , WP.Seq 95 96
  , WP.Var 96 "right"
  , WP.Seq 96 97
  , WP.Var 97 "res"
  , WP.Seq 97 98
  , WP.Assign 98 "value" (Num 0)
  , WP.Seq 98 99
  , WP.Var 99 "value_o"
  , WP.Seq 99 100
  , WP.Branch 100 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 102 102
  , WP.Seq 101 42
  , WP.Seq 101 103
  , WP.Var 102 "NOP_102"
  , WP.Seq 102 103
  , WP.Var 103 "IF_ELSE_FOOTER"
  , WP.Assign 104 "left" (Num 0)
  , WP.Seq 104 105
  , WP.Var 105 "left_o"
  , WP.Seq 105 106
  , WP.Branch 106 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 108 108
  , WP.Seq 107 42
  , WP.Seq 107 109
  , WP.Var 108 "NOP_108"
  , WP.Seq 108 109
  , WP.Var 109 "IF_ELSE_FOOTER"
  , WP.Assign 110 "right" (Num 0)
  , WP.Seq 110 111
  , WP.Var 111 "left_o"
  , WP.Seq 111 112
  , WP.Var 112 "right_o"
  , WP.Seq 112 113
  , WP.Var 113 "dres"
  , WP.Seq 113 114
  , WP.Assign 114 "res" (Num 0)
  , WP.Seq 114 115
  , WP.Branch 115 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 117 117
  , WP.Seq 116 1813
  , WP.Seq 116 118
  , WP.Var 117 "NOP_117"
  , WP.Seq 117 118
  , WP.Var 118 "IF_ELSE_FOOTER"
  , WP.Assign 119 "undefed" (Num 0)
  , WP.Seq 119 120
  , WP.Assign 120 "stack_pointer" (Num 0)
  , WP.Seq 120 121
  , WP.Var 121 "word"
  , WP.Seq 121 122
  , WP.Assign 122 "opcode" (Num 0)
  , WP.Seq 122 123
  , WP.Assign 123 "oparg" (Num 0)
  , WP.Seq 123 124
  , WP.Branch 124 (Eq (Num 0) (Num 1)) 125 128
  , WP.Var 125 "word"
  , WP.Seq 125 126
  , WP.Assign 126 "opcode" (Num 0)
  , WP.Seq 126 127
  , WP.Assign 127 "oparg" (Num 0)
  , WP.Seq 127 128
  , WP.Seq 127 124
  , WP.Var 128 "LOOP_FOOTER"
  , WP.Seq 128 129
  , WP.Seq 128 35
  , WP.Branch 129 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 131 170
  , WP.Var 131 "NOP_131"
  , WP.Var 132 "__CLABEL_TARGET_BINARY_OP_ADD_INT"
  , WP.Seq 132 133
  , WP.Var 133 "this_instr"
  , WP.Seq 133 134
  , WP.Assign 134 "undefed" (Num 0)
  , WP.Seq 134 135
  , WP.Assign 135 "next_instr" (Num 0)
  , WP.Seq 135 136
  , WP.Var 136 "value"
  , WP.Seq 136 137
  , WP.Var 137 "left"
  , WP.Seq 137 138
  , WP.Var 138 "right"
  , WP.Seq 138 139
  , WP.Var 139 "res"
  , WP.Seq 139 140
  , WP.Assign 140 "value" (Num 0)
  , WP.Seq 140 141
  , WP.Var 141 "value_o"
  , WP.Seq 141 142
  , WP.Branch 142 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 144 144
  , WP.Seq 143 42
  , WP.Seq 143 145
  , WP.Var 144 "NOP_144"
  , WP.Seq 144 145
  , WP.Var 145 "IF_ELSE_FOOTER"
  , WP.Assign 146 "left" (Num 0)
  , WP.Seq 146 147
  , WP.Var 147 "left_o"
  , WP.Seq 147 148
  , WP.Branch 148 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 150 150
  , WP.Seq 149 42
  , WP.Seq 149 151
  , WP.Var 150 "NOP_150"
  , WP.Seq 150 151
  , WP.Var 151 "IF_ELSE_FOOTER"
  , WP.Assign 152 "right" (Num 0)
  , WP.Seq 152 153
  , WP.Var 153 "left_o"
  , WP.Seq 153 154
  , WP.Var 154 "right_o"
  , WP.Seq 154 155
  , WP.Assign 155 "res" (Num 0)
  , WP.Seq 155 156
  , WP.Branch 156 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 158 158
  , WP.Seq 157 42
  , WP.Seq 157 159
  , WP.Var 158 "NOP_158"
  , WP.Seq 158 159
  , WP.Var 159 "IF_ELSE_FOOTER"
  , WP.Assign 160 "undefed" (Num 0)
  , WP.Seq 160 161
  , WP.Assign 161 "stack_pointer" (Num 0)
  , WP.Seq 161 162
  , WP.Var 162 "word"
  , WP.Seq 162 163
  , WP.Assign 163 "opcode" (Num 0)
  , WP.Seq 163 164
  , WP.Assign 164 "oparg" (Num 0)
  , WP.Seq 164 165
  , WP.Branch 165 (Eq (Num 0) (Num 1)) 166 169
  , WP.Var 166 "word"
  , WP.Seq 166 167
  , WP.Assign 167 "opcode" (Num 0)
  , WP.Seq 167 168
  , WP.Assign 168 "oparg" (Num 0)
  , WP.Seq 168 169
  , WP.Seq 168 165
  , WP.Var 169 "LOOP_FOOTER"
  , WP.Seq 169 170
  , WP.Seq 169 35
  , WP.Branch 170 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 172 214
  , WP.Var 172 "NOP_172"
  , WP.Var 173 "__CLABEL_TARGET_BINARY_OP_ADD_UNICODE"
  , WP.Seq 173 174
  , WP.Var 174 "this_instr"
  , WP.Seq 174 175
  , WP.Assign 175 "undefed" (Num 0)
  , WP.Seq 175 176
  , WP.Assign 176 "next_instr" (Num 0)
  , WP.Seq 176 177
  , WP.Var 177 "value"
  , WP.Seq 177 178
  , WP.Var 178 "nos"
  , WP.Seq 178 179
  , WP.Var 179 "left"
  , WP.Seq 179 180
  , WP.Var 180 "right"
  , WP.Seq 180 181
  , WP.Var 181 "res"
  , WP.Seq 181 182
  , WP.Assign 182 "value" (Num 0)
  , WP.Seq 182 183
  , WP.Var 183 "value_o"
  , WP.Seq 183 184
  , WP.Branch 184 (Eq (Plus (Plus (Num 0) (Plus (Id "PyUnicode_Type") (Num 0))) (Num 0)) (Num 1)) 186 186
  , WP.Seq 185 42
  , WP.Seq 185 187
  , WP.Var 186 "NOP_186"
  , WP.Seq 186 187
  , WP.Var 187 "IF_ELSE_FOOTER"
  , WP.Assign 188 "nos" (Num 0)
  , WP.Seq 188 189
  , WP.Var 189 "o"
  , WP.Seq 189 190
  , WP.Branch 190 (Eq (Plus (Plus (Num 0) (Plus (Id "PyUnicode_Type") (Num 0))) (Num 0)) (Num 1)) 192 192
  , WP.Seq 191 42
  , WP.Seq 191 193
  , WP.Var 192 "NOP_192"
  , WP.Seq 192 193
  , WP.Var 193 "IF_ELSE_FOOTER"
  , WP.Assign 194 "right" (Num 0)
  , WP.Seq 194 195
  , WP.Assign 195 "left" (Num 0)
  , WP.Seq 195 196
  , WP.Var 196 "left_o"
  , WP.Seq 196 197
  , WP.Var 197 "right_o"
  , WP.Seq 197 198
  , WP.Var 198 "res_o"
  , WP.Seq 198 199
  , WP.Branch 199 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 201 201
  , WP.Seq 200 1813
  , WP.Seq 200 202
  , WP.Var 201 "NOP_201"
  , WP.Seq 201 202
  , WP.Var 202 "IF_ELSE_FOOTER"
  , WP.Assign 203 "res" (Num 0)
  , WP.Seq 203 204
  , WP.Assign 204 "undefed" (Num 0)
  , WP.Seq 204 205
  , WP.Assign 205 "stack_pointer" (Num 0)
  , WP.Seq 205 206
  , WP.Var 206 "word"
  , WP.Seq 206 207
  , WP.Assign 207 "opcode" (Num 0)
  , WP.Seq 207 208
  , WP.Assign 208 "oparg" (Num 0)
  , WP.Seq 208 209
  , WP.Branch 209 (Eq (Num 0) (Num 1)) 210 213
  , WP.Var 210 "word"
  , WP.Seq 210 211
  , WP.Assign 211 "opcode" (Num 0)
  , WP.Seq 211 212
  , WP.Assign 212 "oparg" (Num 0)
  , WP.Seq 212 213
  , WP.Seq 212 209
  , WP.Var 213 "LOOP_FOOTER"
  , WP.Seq 213 214
  , WP.Seq 213 35
  , WP.Branch 214 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 216 260
  , WP.Var 216 "NOP_216"
  , WP.Var 217 "__CLABEL_TARGET_BINARY_OP_EXTEND"
  , WP.Seq 217 218
  , WP.Var 218 "this_instr"
  , WP.Seq 218 219
  , WP.Assign 219 "undefed" (Num 0)
  , WP.Seq 219 220
  , WP.Assign 220 "next_instr" (Num 0)
  , WP.Seq 220 221
  , WP.Var 221 "left"
  , WP.Seq 221 222
  , WP.Var 222 "right"
  , WP.Seq 222 223
  , WP.Var 223 "res"
  , WP.Seq 223 224
  , WP.Assign 224 "right" (Num 0)
  , WP.Seq 224 225
  , WP.Assign 225 "left" (Num 0)
  , WP.Seq 225 226
  , WP.Var 226 "descr"
  , WP.Seq 226 227
  , WP.Var 227 "left_o"
  , WP.Seq 227 228
  , WP.Var 228 "right_o"
  , WP.Seq 228 229
  , WP.Var 229 "d"
  , WP.Seq 229 230
  , WP.Var 230 "res"
  , WP.Seq 230 231
  , WP.Assign 231 "stack_pointer" (Num 0)
  , WP.Seq 231 232
  , WP.Branch 232 (Eq (Plus (Id "res") (Num 0)) (Num 1)) 234 234
  , WP.Seq 233 42
  , WP.Seq 233 235
  , WP.Var 234 "NOP_234"
  , WP.Seq 234 235
  , WP.Var 235 "IF_ELSE_FOOTER"
  , WP.Var 236 "descr"
  , WP.Seq 236 237
  , WP.Var 237 "left_o"
  , WP.Seq 237 238
  , WP.Var 238 "right_o"
  , WP.Seq 238 239
  , WP.Var 239 "d"
  , WP.Seq 239 240
  , WP.Var 240 "res_o"
  , WP.Seq 240 241
  , WP.Var 241 "tmp"
  , WP.Seq 241 242
  , WP.Assign 242 "right" (Num 0)
  , WP.Seq 242 243
  , WP.Assign 243 "undefed" (Num 0)
  , WP.Seq 243 244
  , WP.Assign 244 "tmp" (Num 0)
  , WP.Seq 244 245
  , WP.Assign 245 "left" (Num 0)
  , WP.Seq 245 246
  , WP.Assign 246 "undefed" (Num 0)
  , WP.Seq 246 247
  , WP.Assign 247 "stack_pointer" (Num 0)
  , WP.Seq 247 248
  , WP.Assign 248 "stack_pointer" (Num 0)
  , WP.Seq 248 249
  , WP.Assign 249 "res" (Num 0)
  , WP.Seq 249 250
  , WP.Assign 250 "undefed" (Num 0)
  , WP.Seq 250 251
  , WP.Assign 251 "stack_pointer" (Num 0)
  , WP.Seq 251 252
  , WP.Var 252 "word"
  , WP.Seq 252 253
  , WP.Assign 253 "opcode" (Num 0)
  , WP.Seq 253 254
  , WP.Assign 254 "oparg" (Num 0)
  , WP.Seq 254 255
  , WP.Branch 255 (Eq (Num 0) (Num 1)) 256 259
  , WP.Var 256 "word"
  , WP.Seq 256 257
  , WP.Assign 257 "opcode" (Num 0)
  , WP.Seq 257 258
  , WP.Assign 258 "oparg" (Num 0)
  , WP.Seq 258 259
  , WP.Seq 258 255
  , WP.Var 259 "LOOP_FOOTER"
  , WP.Seq 259 260
  , WP.Seq 259 35
  , WP.Branch 260 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 262 354
  , WP.Var 262 "NOP_262"
  , WP.Var 263 "__CLABEL_TARGET_BINARY_OP_INPLACE_ADD_UNICODE"
  , WP.Seq 263 264
  , WP.Var 264 "this_instr"
  , WP.Seq 264 265
  , WP.Assign 265 "undefed" (Num 0)
  , WP.Seq 265 266
  , WP.Assign 266 "next_instr" (Num 0)
  , WP.Seq 266 267
  , WP.Var 267 "value"
  , WP.Seq 267 268
  , WP.Var 268 "nos"
  , WP.Seq 268 269
  , WP.Var 269 "left"
  , WP.Seq 269 270
  , WP.Var 270 "right"
  , WP.Seq 270 271
  , WP.Assign 271 "value" (Num 0)
  , WP.Seq 271 272
  , WP.Var 272 "value_o"
  , WP.Seq 272 273
  , WP.Branch 273 (Eq (Plus (Plus (Num 0) (Plus (Id "PyUnicode_Type") (Num 0))) (Num 0)) (Num 1)) 275 275
  , WP.Seq 274 42
  , WP.Seq 274 276
  , WP.Var 275 "NOP_275"
  , WP.Seq 275 276
  , WP.Var 276 "IF_ELSE_FOOTER"
  , WP.Assign 277 "nos" (Num 0)
  , WP.Seq 277 278
  , WP.Var 278 "o"
  , WP.Seq 278 279
  , WP.Branch 279 (Eq (Plus (Plus (Num 0) (Plus (Id "PyUnicode_Type") (Num 0))) (Num 0)) (Num 1)) 281 281
  , WP.Seq 280 42
  , WP.Seq 280 282
  , WP.Var 281 "NOP_281"
  , WP.Seq 281 282
  , WP.Var 282 "IF_ELSE_FOOTER"
  , WP.Assign 283 "right" (Num 0)
  , WP.Seq 283 284
  , WP.Assign 284 "left" (Num 0)
  , WP.Seq 284 285
  , WP.Var 285 "left_o"
  , WP.Seq 285 286
  , WP.Var 286 "next_oparg"
  , WP.Seq 286 287
  , WP.Assign 287 "next_oparg" (Num 0)
  , WP.Seq 287 288
  , WP.Var 288 "target_local"
  , WP.Seq 288 289
  , WP.Branch 289 (Eq (Plus (Num 0) (Id "left_o")) (Num 1)) 291 291
  , WP.Seq 290 42
  , WP.Seq 290 292
  , WP.Var 291 "NOP_291"
  , WP.Seq 291 292
  , WP.Var 292 "IF_ELSE_FOOTER"
  , WP.Var 293 "temp"
  , WP.Seq 293 294
  , WP.Var 294 "right_o"
  , WP.Seq 294 295
  , WP.Assign 295 "stack_pointer" (Num 0)
  , WP.Seq 295 296
  , WP.Assign 296 "stack_pointer" (Num 0)
  , WP.Seq 296 297
  , WP.Assign 297 "undefed" (Num 0)
  , WP.Seq 297 298
  , WP.Var 298 "op"
  , WP.Seq 298 299
  , WP.Branch 299 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 301 316
  , WP.Var 301 "tracer"
  , WP.Seq 301 302
  , WP.Branch 302 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 304 305
  , WP.Var 304 "data"
  , WP.Seq 304 305
  , WP.Seq 304 306
  , WP.Var 305 "NOP_305"
  , WP.Seq 305 306
  , WP.Var 306 "IF_ELSE_FOOTER"
  , WP.Branch 307 (Eq (Num 0) (Num 1)) 308 314
  , WP.Var 308 "tracer"
  , WP.Seq 308 309
  , WP.Branch 309 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 311 312
  , WP.Var 311 "data"
  , WP.Seq 311 312
  , WP.Seq 311 313
  , WP.Var 312 "NOP_312"
  , WP.Seq 312 313
  , WP.Var 313 "IF_ELSE_FOOTER"
  , WP.Seq 313 307
  , WP.Var 314 "LOOP_FOOTER"
  , WP.Seq 314 315
  , WP.Var 315 "dealloc"
  , WP.Seq 315 316
  , WP.Seq 315 317
  , WP.Var 316 "NOP_316"
  , WP.Seq 316 317
  , WP.Var 317 "IF_ELSE_FOOTER"
  , WP.Branch 318 (Eq (Num 0) (Num 1)) 319 339
  , WP.Var 319 "op"
  , WP.Seq 319 320
  , WP.Branch 320 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 322 337
  , WP.Var 322 "tracer"
  , WP.Seq 322 323
  , WP.Branch 323 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 325 326
  , WP.Var 325 "data"
  , WP.Seq 325 326
  , WP.Seq 325 327
  , WP.Var 326 "NOP_326"
  , WP.Seq 326 327
  , WP.Var 327 "IF_ELSE_FOOTER"
  , WP.Branch 328 (Eq (Num 0) (Num 1)) 329 335
  , WP.Var 329 "tracer"
  , WP.Seq 329 330
  , WP.Branch 330 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 332 333
  , WP.Var 332 "data"
  , WP.Seq 332 333
  , WP.Seq 332 334
  , WP.Var 333 "NOP_333"
  , WP.Seq 333 334
  , WP.Var 334 "IF_ELSE_FOOTER"
  , WP.Seq 334 328
  , WP.Var 335 "LOOP_FOOTER"
  , WP.Seq 335 336
  , WP.Var 336 "dealloc"
  , WP.Seq 336 337
  , WP.Seq 336 338
  , WP.Var 337 "NOP_337"
  , WP.Seq 337 338
  , WP.Var 338 "IF_ELSE_FOOTER"
  , WP.Seq 338 318
  , WP.Var 339 "LOOP_FOOTER"
  , WP.Seq 339 340
  , WP.Assign 340 "stack_pointer" (Num 0)
  , WP.Seq 340 341
  , WP.Branch 341 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 343 343
  , WP.Seq 342 1817
  , WP.Seq 342 344
  , WP.Var 343 "NOP_343"
  , WP.Seq 343 344
  , WP.Var 344 "IF_ELSE_FOOTER"
  , WP.Assign 345 "next_instr" (Num 0)
  , WP.Seq 345 346
  , WP.Var 346 "word"
  , WP.Seq 346 347
  , WP.Assign 347 "opcode" (Num 0)
  , WP.Seq 347 348
  , WP.Assign 348 "oparg" (Num 0)
  , WP.Seq 348 349
  , WP.Branch 349 (Eq (Num 0) (Num 1)) 350 353
  , WP.Var 350 "word"
  , WP.Seq 350 351
  , WP.Assign 351 "opcode" (Num 0)
  , WP.Seq 351 352
  , WP.Assign 352 "oparg" (Num 0)
  , WP.Seq 352 353
  , WP.Seq 352 349
  , WP.Var 353 "LOOP_FOOTER"
  , WP.Seq 353 354
  , WP.Seq 353 35
  , WP.Branch 354 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 356 396
  , WP.Var 356 "NOP_356"
  , WP.Var 357 "__CLABEL_TARGET_BINARY_OP_MULTIPLY_FLOAT"
  , WP.Seq 357 358
  , WP.Var 358 "this_instr"
  , WP.Seq 358 359
  , WP.Assign 359 "undefed" (Num 0)
  , WP.Seq 359 360
  , WP.Assign 360 "next_instr" (Num 0)
  , WP.Seq 360 361
  , WP.Var 361 "value"
  , WP.Seq 361 362
  , WP.Var 362 "left"
  , WP.Seq 362 363
  , WP.Var 363 "right"
  , WP.Seq 363 364
  , WP.Var 364 "res"
  , WP.Seq 364 365
  , WP.Assign 365 "value" (Num 0)
  , WP.Seq 365 366
  , WP.Var 366 "value_o"
  , WP.Seq 366 367
  , WP.Branch 367 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 369 369
  , WP.Seq 368 42
  , WP.Seq 368 370
  , WP.Var 369 "NOP_369"
  , WP.Seq 369 370
  , WP.Var 370 "IF_ELSE_FOOTER"
  , WP.Assign 371 "left" (Num 0)
  , WP.Seq 371 372
  , WP.Var 372 "left_o"
  , WP.Seq 372 373
  , WP.Branch 373 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 375 375
  , WP.Seq 374 42
  , WP.Seq 374 376
  , WP.Var 375 "NOP_375"
  , WP.Seq 375 376
  , WP.Var 376 "IF_ELSE_FOOTER"
  , WP.Assign 377 "right" (Num 0)
  , WP.Seq 377 378
  , WP.Var 378 "left_o"
  , WP.Seq 378 379
  , WP.Var 379 "right_o"
  , WP.Seq 379 380
  , WP.Var 380 "dres"
  , WP.Seq 380 381
  , WP.Assign 381 "res" (Num 0)
  , WP.Seq 381 382
  , WP.Branch 382 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 384 384
  , WP.Seq 383 1813
  , WP.Seq 383 385
  , WP.Var 384 "NOP_384"
  , WP.Seq 384 385
  , WP.Var 385 "IF_ELSE_FOOTER"
  , WP.Assign 386 "undefed" (Num 0)
  , WP.Seq 386 387
  , WP.Assign 387 "stack_pointer" (Num 0)
  , WP.Seq 387 388
  , WP.Var 388 "word"
  , WP.Seq 388 389
  , WP.Assign 389 "opcode" (Num 0)
  , WP.Seq 389 390
  , WP.Assign 390 "oparg" (Num 0)
  , WP.Seq 390 391
  , WP.Branch 391 (Eq (Num 0) (Num 1)) 392 395
  , WP.Var 392 "word"
  , WP.Seq 392 393
  , WP.Assign 393 "opcode" (Num 0)
  , WP.Seq 393 394
  , WP.Assign 394 "oparg" (Num 0)
  , WP.Seq 394 395
  , WP.Seq 394 391
  , WP.Var 395 "LOOP_FOOTER"
  , WP.Seq 395 396
  , WP.Seq 395 35
  , WP.Branch 396 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 398 437
  , WP.Var 398 "NOP_398"
  , WP.Var 399 "__CLABEL_TARGET_BINARY_OP_MULTIPLY_INT"
  , WP.Seq 399 400
  , WP.Var 400 "this_instr"
  , WP.Seq 400 401
  , WP.Assign 401 "undefed" (Num 0)
  , WP.Seq 401 402
  , WP.Assign 402 "next_instr" (Num 0)
  , WP.Seq 402 403
  , WP.Var 403 "value"
  , WP.Seq 403 404
  , WP.Var 404 "left"
  , WP.Seq 404 405
  , WP.Var 405 "right"
  , WP.Seq 405 406
  , WP.Var 406 "res"
  , WP.Seq 406 407
  , WP.Assign 407 "value" (Num 0)
  , WP.Seq 407 408
  , WP.Var 408 "value_o"
  , WP.Seq 408 409
  , WP.Branch 409 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 411 411
  , WP.Seq 410 42
  , WP.Seq 410 412
  , WP.Var 411 "NOP_411"
  , WP.Seq 411 412
  , WP.Var 412 "IF_ELSE_FOOTER"
  , WP.Assign 413 "left" (Num 0)
  , WP.Seq 413 414
  , WP.Var 414 "left_o"
  , WP.Seq 414 415
  , WP.Branch 415 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 417 417
  , WP.Seq 416 42
  , WP.Seq 416 418
  , WP.Var 417 "NOP_417"
  , WP.Seq 417 418
  , WP.Var 418 "IF_ELSE_FOOTER"
  , WP.Assign 419 "right" (Num 0)
  , WP.Seq 419 420
  , WP.Var 420 "left_o"
  , WP.Seq 420 421
  , WP.Var 421 "right_o"
  , WP.Seq 421 422
  , WP.Assign 422 "res" (Num 0)
  , WP.Seq 422 423
  , WP.Branch 423 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 425 425
  , WP.Seq 424 42
  , WP.Seq 424 426
  , WP.Var 425 "NOP_425"
  , WP.Seq 425 426
  , WP.Var 426 "IF_ELSE_FOOTER"
  , WP.Assign 427 "undefed" (Num 0)
  , WP.Seq 427 428
  , WP.Assign 428 "stack_pointer" (Num 0)
  , WP.Seq 428 429
  , WP.Var 429 "word"
  , WP.Seq 429 430
  , WP.Assign 430 "opcode" (Num 0)
  , WP.Seq 430 431
  , WP.Assign 431 "oparg" (Num 0)
  , WP.Seq 431 432
  , WP.Branch 432 (Eq (Num 0) (Num 1)) 433 436
  , WP.Var 433 "word"
  , WP.Seq 433 434
  , WP.Assign 434 "opcode" (Num 0)
  , WP.Seq 434 435
  , WP.Assign 435 "oparg" (Num 0)
  , WP.Seq 435 436
  , WP.Seq 435 432
  , WP.Var 436 "LOOP_FOOTER"
  , WP.Seq 436 437
  , WP.Seq 436 35
  , WP.Branch 437 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 439 489
  , WP.Var 439 "NOP_439"
  , WP.Var 440 "__CLABEL_TARGET_BINARY_OP_SUBSCR_DICT"
  , WP.Seq 440 441
  , WP.Var 441 "this_instr"
  , WP.Seq 441 442
  , WP.Assign 442 "undefed" (Num 0)
  , WP.Seq 442 443
  , WP.Assign 443 "next_instr" (Num 0)
  , WP.Seq 443 444
  , WP.Var 444 "nos"
  , WP.Seq 444 445
  , WP.Var 445 "dict_st"
  , WP.Seq 445 446
  , WP.Var 446 "sub_st"
  , WP.Seq 446 447
  , WP.Var 447 "res"
  , WP.Seq 447 448
  , WP.Assign 448 "nos" (Num 0)
  , WP.Seq 448 449
  , WP.Var 449 "o"
  , WP.Seq 449 450
  , WP.Branch 450 (Eq (Plus (Plus (Num 0) (Plus (Id "PyDict_Type") (Num 0))) (Num 0)) (Num 1)) 452 452
  , WP.Seq 451 42
  , WP.Seq 451 453
  , WP.Var 452 "NOP_452"
  , WP.Seq 452 453
  , WP.Var 453 "IF_ELSE_FOOTER"
  , WP.Assign 454 "sub_st" (Num 0)
  , WP.Seq 454 455
  , WP.Assign 455 "dict_st" (Num 0)
  , WP.Seq 455 456
  , WP.Var 456 "sub"
  , WP.Seq 456 457
  , WP.Var 457 "dict"
  , WP.Seq 457 458
  , WP.Var 458 "res_o"
  , WP.Seq 458 459
  , WP.Var 459 "rc"
  , WP.Seq 459 460
  , WP.Assign 460 "stack_pointer" (Num 0)
  , WP.Seq 460 461
  , WP.Branch 461 (Eq (Plus (Id "rc") (Num 0)) (Num 1)) 463 464
  , WP.Assign 463 "stack_pointer" (Num 0)
  , WP.Seq 463 465
  , WP.Var 464 "NOP_464"
  , WP.Seq 464 465
  , WP.Var 465 "IF_ELSE_FOOTER"
  , WP.Var 466 "tmp"
  , WP.Seq 466 467
  , WP.Assign 467 "sub_st" (Num 0)
  , WP.Seq 467 468
  , WP.Assign 468 "undefed" (Num 0)
  , WP.Seq 468 469
  , WP.Assign 469 "tmp" (Num 0)
  , WP.Seq 469 470
  , WP.Assign 470 "dict_st" (Num 0)
  , WP.Seq 470 471
  , WP.Assign 471 "undefed" (Num 0)
  , WP.Seq 471 472
  , WP.Assign 472 "stack_pointer" (Num 0)
  , WP.Seq 472 473
  , WP.Assign 473 "stack_pointer" (Num 0)
  , WP.Seq 473 474
  , WP.Branch 474 (Eq (Plus (Id "rc") (Num 0)) (Num 1)) 476 476
  , WP.Seq 475 1817
  , WP.Seq 475 477
  , WP.Var 476 "NOP_476"
  , WP.Seq 476 477
  , WP.Var 477 "IF_ELSE_FOOTER"
  , WP.Assign 478 "res" (Num 0)
  , WP.Seq 478 479
  , WP.Assign 479 "undefed" (Num 0)
  , WP.Seq 479 480
  , WP.Assign 480 "stack_pointer" (Num 0)
  , WP.Seq 480 481
  , WP.Var 481 "word"
  , WP.Seq 481 482
  , WP.Assign 482 "opcode" (Num 0)
  , WP.Seq 482 483
  , WP.Assign 483 "oparg" (Num 0)
  , WP.Seq 483 484
  , WP.Branch 484 (Eq (Num 0) (Num 1)) 485 488
  , WP.Var 485 "word"
  , WP.Seq 485 486
  , WP.Assign 486 "opcode" (Num 0)
  , WP.Seq 486 487
  , WP.Assign 487 "oparg" (Num 0)
  , WP.Seq 487 488
  , WP.Seq 487 484
  , WP.Var 488 "LOOP_FOOTER"
  , WP.Seq 488 489
  , WP.Seq 488 35
  , WP.Branch 489 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 491 549
  , WP.Var 491 "NOP_491"
  , WP.Var 492 "__CLABEL_TARGET_BINARY_OP_SUBSCR_GETITEM"
  , WP.Seq 492 493
  , WP.Var 493 "this_instr"
  , WP.Seq 493 494
  , WP.Assign 494 "undefed" (Num 0)
  , WP.Seq 494 495
  , WP.Assign 495 "next_instr" (Num 0)
  , WP.Seq 495 496
  , WP.Var 496 "container"
  , WP.Seq 496 497
  , WP.Var 497 "getitem"
  , WP.Seq 497 498
  , WP.Var 498 "sub"
  , WP.Seq 498 499
  , WP.Var 499 "new_frame"
  , WP.Seq 499 500
  , WP.Branch 500 (Eq (Num 0) (Num 1)) 502 502
  , WP.Seq 501 42
  , WP.Seq 501 503
  , WP.Var 502 "NOP_502"
  , WP.Seq 502 503
  , WP.Var 503 "IF_ELSE_FOOTER"
  , WP.Assign 504 "container" (Num 0)
  , WP.Seq 504 505
  , WP.Var 505 "tp"
  , WP.Seq 505 506
  , WP.Branch 506 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 508 508
  , WP.Seq 507 42
  , WP.Seq 507 509
  , WP.Var 508 "NOP_508"
  , WP.Seq 508 509
  , WP.Var 509 "IF_ELSE_FOOTER"
  , WP.Var 510 "ht"
  , WP.Seq 510 511
  , WP.Var 511 "getitem_o"
  , WP.Seq 511 512
  , WP.Branch 512 (Eq (Plus (Id "getitem_o") (Num 0)) (Num 1)) 514 514
  , WP.Seq 513 42
  , WP.Seq 513 515
  , WP.Var 514 "NOP_514"
  , WP.Seq 514 515
  , WP.Var 515 "IF_ELSE_FOOTER"
  , WP.Var 516 "cached_version"
  , WP.Seq 516 517
  , WP.Branch 517 (Eq (Plus (Num 0) (Id "cached_version")) (Num 1)) 519 519
  , WP.Seq 518 42
  , WP.Seq 518 520
  , WP.Var 519 "NOP_519"
  , WP.Seq 519 520
  , WP.Var 520 "IF_ELSE_FOOTER"
  , WP.Var 521 "code"
  , WP.Seq 521 522
  , WP.Branch 522 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 524 524
  , WP.Seq 523 42
  , WP.Seq 523 525
  , WP.Var 524 "NOP_524"
  , WP.Seq 524 525
  , WP.Var 525 "IF_ELSE_FOOTER"
  , WP.Assign 526 "getitem" (Num 0)
  , WP.Seq 526 527
  , WP.Assign 527 "sub" (Num 0)
  , WP.Seq 527 528
  , WP.Var 528 "pushed_frame"
  , WP.Seq 528 529
  , WP.Assign 529 "undefed" (Num 0)
  , WP.Seq 529 530
  , WP.Assign 530 "undefed" (Num 0)
  , WP.Seq 530 531
  , WP.Assign 531 "undefed" (Num 0)
  , WP.Seq 531 532
  , WP.Assign 532 "new_frame" (Num 0)
  , WP.Seq 532 533
  , WP.Var 533 "temp"
  , WP.Seq 533 534
  , WP.Assign 534 "stack_pointer" (Num 0)
  , WP.Seq 534 535
  , WP.Assign 535 "frame" (Num 0)
  , WP.Seq 535 536
  , WP.Assign 536 "stack_pointer" (Num 0)
  , WP.Seq 536 537
  , WP.Assign 537 "next_instr" (Num 0)
  , WP.Seq 537 538
  , WP.Branch 538 (Eq (Num 0) (Num 1)) 539 540
  , WP.Assign 539 "next_instr" (Num 0)
  , WP.Seq 539 538
  , WP.Var 540 "LOOP_FOOTER"
  , WP.Seq 540 541
  , WP.Var 541 "word"
  , WP.Seq 541 542
  , WP.Assign 542 "opcode" (Num 0)
  , WP.Seq 542 543
  , WP.Assign 543 "oparg" (Num 0)
  , WP.Seq 543 544
  , WP.Branch 544 (Eq (Num 0) (Num 1)) 545 548
  , WP.Var 545 "word"
  , WP.Seq 545 546
  , WP.Assign 546 "opcode" (Num 0)
  , WP.Seq 546 547
  , WP.Assign 547 "oparg" (Num 0)
  , WP.Seq 547 548
  , WP.Seq 547 544
  , WP.Var 548 "LOOP_FOOTER"
  , WP.Seq 548 549
  , WP.Seq 548 35
  , WP.Branch 549 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 551 604
  , WP.Var 551 "NOP_551"
  , WP.Var 552 "__CLABEL_TARGET_BINARY_OP_SUBSCR_LIST_INT"
  , WP.Seq 552 553
  , WP.Var 553 "this_instr"
  , WP.Seq 553 554
  , WP.Assign 554 "undefed" (Num 0)
  , WP.Seq 554 555
  , WP.Assign 555 "next_instr" (Num 0)
  , WP.Seq 555 556
  , WP.Var 556 "value"
  , WP.Seq 556 557
  , WP.Var 557 "nos"
  , WP.Seq 557 558
  , WP.Var 558 "list_st"
  , WP.Seq 558 559
  , WP.Var 559 "sub_st"
  , WP.Seq 559 560
  , WP.Var 560 "res"
  , WP.Seq 560 561
  , WP.Assign 561 "value" (Num 0)
  , WP.Seq 561 562
  , WP.Var 562 "value_o"
  , WP.Seq 562 563
  , WP.Branch 563 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 565 565
  , WP.Seq 564 42
  , WP.Seq 564 566
  , WP.Var 565 "NOP_565"
  , WP.Seq 565 566
  , WP.Var 566 "IF_ELSE_FOOTER"
  , WP.Assign 567 "nos" (Num 0)
  , WP.Seq 567 568
  , WP.Var 568 "o"
  , WP.Seq 568 569
  , WP.Branch 569 (Eq (Plus (Plus (Num 0) (Plus (Id "PyList_Type") (Num 0))) (Num 0)) (Num 1)) 571 571
  , WP.Seq 570 42
  , WP.Seq 570 572
  , WP.Var 571 "NOP_571"
  , WP.Seq 571 572
  , WP.Var 572 "IF_ELSE_FOOTER"
  , WP.Assign 573 "sub_st" (Num 0)
  , WP.Seq 573 574
  , WP.Assign 574 "list_st" (Num 0)
  , WP.Seq 574 575
  , WP.Var 575 "sub"
  , WP.Seq 575 576
  , WP.Var 576 "list"
  , WP.Seq 576 577
  , WP.Branch 577 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 579 579
  , WP.Seq 578 42
  , WP.Seq 578 580
  , WP.Var 579 "NOP_579"
  , WP.Seq 579 580
  , WP.Var 580 "IF_ELSE_FOOTER"
  , WP.Var 581 "index"
  , WP.Seq 581 582
  , WP.Branch 582 (Eq (Plus (Id "index") (Num 0)) (Num 1)) 584 584
  , WP.Seq 583 42
  , WP.Seq 583 585
  , WP.Var 584 "NOP_584"
  , WP.Seq 584 585
  , WP.Var 585 "IF_ELSE_FOOTER"
  , WP.Var 586 "res_o"
  , WP.Seq 586 587
  , WP.Assign 587 "res" (Num 0)
  , WP.Seq 587 588
  , WP.Var 588 "tmp"
  , WP.Seq 588 589
  , WP.Assign 589 "list_st" (Num 0)
  , WP.Seq 589 590
  , WP.Assign 590 "undefed" (Num 0)
  , WP.Seq 590 591
  , WP.Assign 591 "tmp" (Num 0)
  , WP.Seq 591 592
  , WP.Assign 592 "sub_st" (Num 0)
  , WP.Seq 592 593
  , WP.Assign 593 "undefed" (Num 0)
  , WP.Seq 593 594
  , WP.Assign 594 "stack_pointer" (Num 0)
  , WP.Seq 594 595
  , WP.Assign 595 "stack_pointer" (Num 0)
  , WP.Seq 595 596
  , WP.Var 596 "word"
  , WP.Seq 596 597
  , WP.Assign 597 "opcode" (Num 0)
  , WP.Seq 597 598
  , WP.Assign 598 "oparg" (Num 0)
  , WP.Seq 598 599
  , WP.Branch 599 (Eq (Num 0) (Num 1)) 600 603
  , WP.Var 600 "word"
  , WP.Seq 600 601
  , WP.Assign 601 "opcode" (Num 0)
  , WP.Seq 601 602
  , WP.Assign 602 "oparg" (Num 0)
  , WP.Seq 602 603
  , WP.Seq 602 599
  , WP.Var 603 "LOOP_FOOTER"
  , WP.Seq 603 604
  , WP.Seq 603 35
  , WP.Branch 604 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 606 657
  , WP.Var 606 "NOP_606"
  , WP.Var 607 "__CLABEL_TARGET_BINARY_OP_SUBSCR_LIST_SLICE"
  , WP.Seq 607 608
  , WP.Var 608 "this_instr"
  , WP.Seq 608 609
  , WP.Assign 609 "undefed" (Num 0)
  , WP.Seq 609 610
  , WP.Assign 610 "next_instr" (Num 0)
  , WP.Seq 610 611
  , WP.Var 611 "tos"
  , WP.Seq 611 612
  , WP.Var 612 "nos"
  , WP.Seq 612 613
  , WP.Var 613 "list_st"
  , WP.Seq 613 614
  , WP.Var 614 "sub_st"
  , WP.Seq 614 615
  , WP.Var 615 "res"
  , WP.Seq 615 616
  , WP.Assign 616 "tos" (Num 0)
  , WP.Seq 616 617
  , WP.Var 617 "o"
  , WP.Seq 617 618
  , WP.Branch 618 (Eq (Plus (Plus (Num 0) (Plus (Id "PySlice_Type") (Num 0))) (Num 0)) (Num 1)) 620 620
  , WP.Seq 619 42
  , WP.Seq 619 621
  , WP.Var 620 "NOP_620"
  , WP.Seq 620 621
  , WP.Var 621 "IF_ELSE_FOOTER"
  , WP.Assign 622 "nos" (Num 0)
  , WP.Seq 622 623
  , WP.Var 623 "o"
  , WP.Seq 623 624
  , WP.Branch 624 (Eq (Plus (Plus (Num 0) (Plus (Id "PyList_Type") (Num 0))) (Num 0)) (Num 1)) 626 626
  , WP.Seq 625 42
  , WP.Seq 625 627
  , WP.Var 626 "NOP_626"
  , WP.Seq 626 627
  , WP.Var 627 "IF_ELSE_FOOTER"
  , WP.Assign 628 "sub_st" (Num 0)
  , WP.Seq 628 629
  , WP.Assign 629 "list_st" (Num 0)
  , WP.Seq 629 630
  , WP.Var 630 "sub"
  , WP.Seq 630 631
  , WP.Var 631 "list"
  , WP.Seq 631 632
  , WP.Var 632 "res_o"
  , WP.Seq 632 633
  , WP.Assign 633 "stack_pointer" (Num 0)
  , WP.Seq 633 634
  , WP.Var 634 "tmp"
  , WP.Seq 634 635
  , WP.Assign 635 "sub_st" (Num 0)
  , WP.Seq 635 636
  , WP.Assign 636 "undefed" (Num 0)
  , WP.Seq 636 637
  , WP.Assign 637 "tmp" (Num 0)
  , WP.Seq 637 638
  , WP.Assign 638 "list_st" (Num 0)
  , WP.Seq 638 639
  , WP.Assign 639 "undefed" (Num 0)
  , WP.Seq 639 640
  , WP.Assign 640 "stack_pointer" (Num 0)
  , WP.Seq 640 641
  , WP.Assign 641 "stack_pointer" (Num 0)
  , WP.Seq 641 642
  , WP.Branch 642 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 644 644
  , WP.Seq 643 1817
  , WP.Seq 643 645
  , WP.Var 644 "NOP_644"
  , WP.Seq 644 645
  , WP.Var 645 "IF_ELSE_FOOTER"
  , WP.Assign 646 "res" (Num 0)
  , WP.Seq 646 647
  , WP.Assign 647 "undefed" (Num 0)
  , WP.Seq 647 648
  , WP.Assign 648 "stack_pointer" (Num 0)
  , WP.Seq 648 649
  , WP.Var 649 "word"
  , WP.Seq 649 650
  , WP.Assign 650 "opcode" (Num 0)
  , WP.Seq 650 651
  , WP.Assign 651 "oparg" (Num 0)
  , WP.Seq 651 652
  , WP.Branch 652 (Eq (Num 0) (Num 1)) 653 656
  , WP.Var 653 "word"
  , WP.Seq 653 654
  , WP.Assign 654 "opcode" (Num 0)
  , WP.Seq 654 655
  , WP.Assign 655 "oparg" (Num 0)
  , WP.Seq 655 656
  , WP.Seq 655 652
  , WP.Var 656 "LOOP_FOOTER"
  , WP.Seq 656 657
  , WP.Seq 656 35
  , WP.Branch 657 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 659 713
  , WP.Var 659 "NOP_659"
  , WP.Var 660 "__CLABEL_TARGET_BINARY_OP_SUBSCR_STR_INT"
  , WP.Seq 660 661
  , WP.Var 661 "this_instr"
  , WP.Seq 661 662
  , WP.Assign 662 "undefed" (Num 0)
  , WP.Seq 662 663
  , WP.Assign 663 "next_instr" (Num 0)
  , WP.Seq 663 664
  , WP.Var 664 "value"
  , WP.Seq 664 665
  , WP.Var 665 "nos"
  , WP.Seq 665 666
  , WP.Var 666 "str_st"
  , WP.Seq 666 667
  , WP.Var 667 "sub_st"
  , WP.Seq 667 668
  , WP.Var 668 "res"
  , WP.Seq 668 669
  , WP.Assign 669 "value" (Num 0)
  , WP.Seq 669 670
  , WP.Var 670 "value_o"
  , WP.Seq 670 671
  , WP.Branch 671 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 673 673
  , WP.Seq 672 42
  , WP.Seq 672 674
  , WP.Var 673 "NOP_673"
  , WP.Seq 673 674
  , WP.Var 674 "IF_ELSE_FOOTER"
  , WP.Assign 675 "nos" (Num 0)
  , WP.Seq 675 676
  , WP.Var 676 "o"
  , WP.Seq 676 677
  , WP.Branch 677 (Eq (Plus (Plus (Num 0) (Plus (Id "PyUnicode_Type") (Num 0))) (Num 0)) (Num 1)) 679 679
  , WP.Seq 678 42
  , WP.Seq 678 680
  , WP.Var 679 "NOP_679"
  , WP.Seq 679 680
  , WP.Var 680 "IF_ELSE_FOOTER"
  , WP.Assign 681 "sub_st" (Num 0)
  , WP.Seq 681 682
  , WP.Assign 682 "str_st" (Num 0)
  , WP.Seq 682 683
  , WP.Var 683 "sub"
  , WP.Seq 683 684
  , WP.Var 684 "str"
  , WP.Seq 684 685
  , WP.Branch 685 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 687 687
  , WP.Seq 686 42
  , WP.Seq 686 688
  , WP.Var 687 "NOP_687"
  , WP.Seq 687 688
  , WP.Var 688 "IF_ELSE_FOOTER"
  , WP.Var 689 "index"
  , WP.Seq 689 690
  , WP.Branch 690 (Eq (Plus (Num 0) (Id "index")) (Num 1)) 692 692
  , WP.Seq 691 42
  , WP.Seq 691 693
  , WP.Var 692 "NOP_692"
  , WP.Seq 692 693
  , WP.Var 693 "IF_ELSE_FOOTER"
  , WP.Var 694 "c"
  , WP.Seq 694 695
  , WP.Branch 695 (Eq (Plus (Plus (Plus (Num 0) (Num 0)) (Plus (Num 0) (Num 0))) (Id "c")) (Num 1)) 697 697
  , WP.Seq 696 42
  , WP.Seq 696 698
  , WP.Var 697 "NOP_697"
  , WP.Seq 697 698
  , WP.Var 698 "IF_ELSE_FOOTER"
  , WP.Var 699 "res_o"
  , WP.Seq 699 700
  , WP.Assign 700 "stack_pointer" (Num 0)
  , WP.Seq 700 701
  , WP.Assign 701 "stack_pointer" (Num 0)
  , WP.Seq 701 702
  , WP.Assign 702 "res" (Num 0)
  , WP.Seq 702 703
  , WP.Assign 703 "undefed" (Num 0)
  , WP.Seq 703 704
  , WP.Assign 704 "stack_pointer" (Num 0)
  , WP.Seq 704 705
  , WP.Var 705 "word"
  , WP.Seq 705 706
  , WP.Assign 706 "opcode" (Num 0)
  , WP.Seq 706 707
  , WP.Assign 707 "oparg" (Num 0)
  , WP.Seq 707 708
  , WP.Branch 708 (Eq (Num 0) (Num 1)) 709 712
  , WP.Var 709 "word"
  , WP.Seq 709 710
  , WP.Assign 710 "opcode" (Num 0)
  , WP.Seq 710 711
  , WP.Assign 711 "oparg" (Num 0)
  , WP.Seq 711 712
  , WP.Seq 711 708
  , WP.Var 712 "LOOP_FOOTER"
  , WP.Seq 712 713
  , WP.Seq 712 35
  , WP.Branch 713 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 715 765
  , WP.Var 715 "NOP_715"
  , WP.Var 716 "__CLABEL_TARGET_BINARY_OP_SUBSCR_TUPLE_INT"
  , WP.Seq 716 717
  , WP.Var 717 "this_instr"
  , WP.Seq 717 718
  , WP.Assign 718 "undefed" (Num 0)
  , WP.Seq 718 719
  , WP.Assign 719 "next_instr" (Num 0)
  , WP.Seq 719 720
  , WP.Var 720 "value"
  , WP.Seq 720 721
  , WP.Var 721 "nos"
  , WP.Seq 721 722
  , WP.Var 722 "tuple_st"
  , WP.Seq 722 723
  , WP.Var 723 "sub_st"
  , WP.Seq 723 724
  , WP.Var 724 "res"
  , WP.Seq 724 725
  , WP.Assign 725 "value" (Num 0)
  , WP.Seq 725 726
  , WP.Var 726 "value_o"
  , WP.Seq 726 727
  , WP.Branch 727 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 729 729
  , WP.Seq 728 42
  , WP.Seq 728 730
  , WP.Var 729 "NOP_729"
  , WP.Seq 729 730
  , WP.Var 730 "IF_ELSE_FOOTER"
  , WP.Assign 731 "nos" (Num 0)
  , WP.Seq 731 732
  , WP.Var 732 "o"
  , WP.Seq 732 733
  , WP.Branch 733 (Eq (Plus (Plus (Num 0) (Plus (Id "PyTuple_Type") (Num 0))) (Num 0)) (Num 1)) 735 735
  , WP.Seq 734 42
  , WP.Seq 734 736
  , WP.Var 735 "NOP_735"
  , WP.Seq 735 736
  , WP.Var 736 "IF_ELSE_FOOTER"
  , WP.Assign 737 "sub_st" (Num 0)
  , WP.Seq 737 738
  , WP.Assign 738 "tuple_st" (Num 0)
  , WP.Seq 738 739
  , WP.Var 739 "sub"
  , WP.Seq 739 740
  , WP.Var 740 "tuple"
  , WP.Seq 740 741
  , WP.Branch 741 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 743 743
  , WP.Seq 742 42
  , WP.Seq 742 744
  , WP.Var 743 "NOP_743"
  , WP.Seq 743 744
  , WP.Var 744 "IF_ELSE_FOOTER"
  , WP.Var 745 "index"
  , WP.Seq 745 746
  , WP.Branch 746 (Eq (Plus (Id "index") (Num 0)) (Num 1)) 748 748
  , WP.Seq 747 42
  , WP.Seq 747 749
  , WP.Var 748 "NOP_748"
  , WP.Seq 748 749
  , WP.Var 749 "IF_ELSE_FOOTER"
  , WP.Var 750 "res_o"
  , WP.Seq 750 751
  , WP.Assign 751 "res" (Num 0)
  , WP.Seq 751 752
  , WP.Assign 752 "stack_pointer" (Num 0)
  , WP.Seq 752 753
  , WP.Var 753 "tmp"
  , WP.Seq 753 754
  , WP.Assign 754 "tuple_st" (Num 0)
  , WP.Seq 754 755
  , WP.Assign 755 "undefed" (Num 0)
  , WP.Seq 755 756
  , WP.Assign 756 "stack_pointer" (Num 0)
  , WP.Seq 756 757
  , WP.Var 757 "word"
  , WP.Seq 757 758
  , WP.Assign 758 "opcode" (Num 0)
  , WP.Seq 758 759
  , WP.Assign 759 "oparg" (Num 0)
  , WP.Seq 759 760
  , WP.Branch 760 (Eq (Num 0) (Num 1)) 761 764
  , WP.Var 761 "word"
  , WP.Seq 761 762
  , WP.Assign 762 "opcode" (Num 0)
  , WP.Seq 762 763
  , WP.Assign 763 "oparg" (Num 0)
  , WP.Seq 763 764
  , WP.Seq 763 760
  , WP.Var 764 "LOOP_FOOTER"
  , WP.Seq 764 765
  , WP.Seq 764 35
  , WP.Branch 765 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 767 807
  , WP.Var 767 "NOP_767"
  , WP.Var 768 "__CLABEL_TARGET_BINARY_OP_SUBTRACT_FLOAT"
  , WP.Seq 768 769
  , WP.Var 769 "this_instr"
  , WP.Seq 769 770
  , WP.Assign 770 "undefed" (Num 0)
  , WP.Seq 770 771
  , WP.Assign 771 "next_instr" (Num 0)
  , WP.Seq 771 772
  , WP.Var 772 "value"
  , WP.Seq 772 773
  , WP.Var 773 "left"
  , WP.Seq 773 774
  , WP.Var 774 "right"
  , WP.Seq 774 775
  , WP.Var 775 "res"
  , WP.Seq 775 776
  , WP.Assign 776 "value" (Num 0)
  , WP.Seq 776 777
  , WP.Var 777 "value_o"
  , WP.Seq 777 778
  , WP.Branch 778 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 780 780
  , WP.Seq 779 42
  , WP.Seq 779 781
  , WP.Var 780 "NOP_780"
  , WP.Seq 780 781
  , WP.Var 781 "IF_ELSE_FOOTER"
  , WP.Assign 782 "left" (Num 0)
  , WP.Seq 782 783
  , WP.Var 783 "left_o"
  , WP.Seq 783 784
  , WP.Branch 784 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 786 786
  , WP.Seq 785 42
  , WP.Seq 785 787
  , WP.Var 786 "NOP_786"
  , WP.Seq 786 787
  , WP.Var 787 "IF_ELSE_FOOTER"
  , WP.Assign 788 "right" (Num 0)
  , WP.Seq 788 789
  , WP.Var 789 "left_o"
  , WP.Seq 789 790
  , WP.Var 790 "right_o"
  , WP.Seq 790 791
  , WP.Var 791 "dres"
  , WP.Seq 791 792
  , WP.Assign 792 "res" (Num 0)
  , WP.Seq 792 793
  , WP.Branch 793 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 795 795
  , WP.Seq 794 1813
  , WP.Seq 794 796
  , WP.Var 795 "NOP_795"
  , WP.Seq 795 796
  , WP.Var 796 "IF_ELSE_FOOTER"
  , WP.Assign 797 "undefed" (Num 0)
  , WP.Seq 797 798
  , WP.Assign 798 "stack_pointer" (Num 0)
  , WP.Seq 798 799
  , WP.Var 799 "word"
  , WP.Seq 799 800
  , WP.Assign 800 "opcode" (Num 0)
  , WP.Seq 800 801
  , WP.Assign 801 "oparg" (Num 0)
  , WP.Seq 801 802
  , WP.Branch 802 (Eq (Num 0) (Num 1)) 803 806
  , WP.Var 803 "word"
  , WP.Seq 803 804
  , WP.Assign 804 "opcode" (Num 0)
  , WP.Seq 804 805
  , WP.Assign 805 "oparg" (Num 0)
  , WP.Seq 805 806
  , WP.Seq 805 802
  , WP.Var 806 "LOOP_FOOTER"
  , WP.Seq 806 807
  , WP.Seq 806 35
  , WP.Branch 807 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 809 848
  , WP.Var 809 "NOP_809"
  , WP.Var 810 "__CLABEL_TARGET_BINARY_OP_SUBTRACT_INT"
  , WP.Seq 810 811
  , WP.Var 811 "this_instr"
  , WP.Seq 811 812
  , WP.Assign 812 "undefed" (Num 0)
  , WP.Seq 812 813
  , WP.Assign 813 "next_instr" (Num 0)
  , WP.Seq 813 814
  , WP.Var 814 "value"
  , WP.Seq 814 815
  , WP.Var 815 "left"
  , WP.Seq 815 816
  , WP.Var 816 "right"
  , WP.Seq 816 817
  , WP.Var 817 "res"
  , WP.Seq 817 818
  , WP.Assign 818 "value" (Num 0)
  , WP.Seq 818 819
  , WP.Var 819 "value_o"
  , WP.Seq 819 820
  , WP.Branch 820 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 822 822
  , WP.Seq 821 42
  , WP.Seq 821 823
  , WP.Var 822 "NOP_822"
  , WP.Seq 822 823
  , WP.Var 823 "IF_ELSE_FOOTER"
  , WP.Assign 824 "left" (Num 0)
  , WP.Seq 824 825
  , WP.Var 825 "left_o"
  , WP.Seq 825 826
  , WP.Branch 826 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 828 828
  , WP.Seq 827 42
  , WP.Seq 827 829
  , WP.Var 828 "NOP_828"
  , WP.Seq 828 829
  , WP.Var 829 "IF_ELSE_FOOTER"
  , WP.Assign 830 "right" (Num 0)
  , WP.Seq 830 831
  , WP.Var 831 "left_o"
  , WP.Seq 831 832
  , WP.Var 832 "right_o"
  , WP.Seq 832 833
  , WP.Assign 833 "res" (Num 0)
  , WP.Seq 833 834
  , WP.Branch 834 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 836 836
  , WP.Seq 835 42
  , WP.Seq 835 837
  , WP.Var 836 "NOP_836"
  , WP.Seq 836 837
  , WP.Var 837 "IF_ELSE_FOOTER"
  , WP.Assign 838 "undefed" (Num 0)
  , WP.Seq 838 839
  , WP.Assign 839 "stack_pointer" (Num 0)
  , WP.Seq 839 840
  , WP.Var 840 "word"
  , WP.Seq 840 841
  , WP.Assign 841 "opcode" (Num 0)
  , WP.Seq 841 842
  , WP.Assign 842 "oparg" (Num 0)
  , WP.Seq 842 843
  , WP.Branch 843 (Eq (Num 0) (Num 1)) 844 847
  , WP.Var 844 "word"
  , WP.Seq 844 845
  , WP.Assign 845 "opcode" (Num 0)
  , WP.Seq 845 846
  , WP.Assign 846 "oparg" (Num 0)
  , WP.Seq 846 847
  , WP.Seq 846 843
  , WP.Var 847 "LOOP_FOOTER"
  , WP.Seq 847 848
  , WP.Seq 847 35
  , WP.Branch 848 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 850 931
  , WP.Var 850 "NOP_850"
  , WP.Var 851 "__CLABEL_TARGET_BINARY_SLICE"
  , WP.Seq 851 852
  , WP.Assign 852 "undefed" (Num 0)
  , WP.Seq 852 853
  , WP.Assign 853 "next_instr" (Num 0)
  , WP.Seq 853 854
  , WP.Var 854 "container"
  , WP.Seq 854 855
  , WP.Var 855 "start"
  , WP.Seq 855 856
  , WP.Var 856 "stop"
  , WP.Seq 856 857
  , WP.Var 857 "res"
  , WP.Seq 857 858
  , WP.Assign 858 "stop" (Num 0)
  , WP.Seq 858 859
  , WP.Assign 859 "start" (Num 0)
  , WP.Seq 859 860
  , WP.Assign 860 "container" (Num 0)
  , WP.Seq 860 861
  , WP.Var 861 "slice"
  , WP.Seq 861 862
  , WP.Assign 862 "stack_pointer" (Num 0)
  , WP.Seq 862 863
  , WP.Var 863 "res_o"
  , WP.Seq 863 864
  , WP.Branch 864 (Eq (Plus (Id "slice") (Num 0)) (Num 1)) 866 867
  , WP.Assign 866 "res_o" (Num 0)
  , WP.Seq 866 913
  , WP.Assign 867 "stack_pointer" (Num 0)
  , WP.Seq 867 868
  , WP.Assign 868 "res_o" (Num 0)
  , WP.Seq 868 869
  , WP.Var 869 "op"
  , WP.Seq 869 870
  , WP.Branch 870 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 872 887
  , WP.Var 872 "tracer"
  , WP.Seq 872 873
  , WP.Branch 873 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 875 876
  , WP.Var 875 "data"
  , WP.Seq 875 876
  , WP.Seq 875 877
  , WP.Var 876 "NOP_876"
  , WP.Seq 876 877
  , WP.Var 877 "IF_ELSE_FOOTER"
  , WP.Branch 878 (Eq (Num 0) (Num 1)) 879 885
  , WP.Var 879 "tracer"
  , WP.Seq 879 880
  , WP.Branch 880 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 882 883
  , WP.Var 882 "data"
  , WP.Seq 882 883
  , WP.Seq 882 884
  , WP.Var 883 "NOP_883"
  , WP.Seq 883 884
  , WP.Var 884 "IF_ELSE_FOOTER"
  , WP.Seq 884 878
  , WP.Var 885 "LOOP_FOOTER"
  , WP.Seq 885 886
  , WP.Var 886 "dealloc"
  , WP.Seq 886 887
  , WP.Seq 886 888
  , WP.Var 887 "NOP_887"
  , WP.Seq 887 888
  , WP.Var 888 "IF_ELSE_FOOTER"
  , WP.Branch 889 (Eq (Num 0) (Num 1)) 890 910
  , WP.Var 890 "op"
  , WP.Seq 890 891
  , WP.Branch 891 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 893 908
  , WP.Var 893 "tracer"
  , WP.Seq 893 894
  , WP.Branch 894 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 896 897
  , WP.Var 896 "data"
  , WP.Seq 896 897
  , WP.Seq 896 898
  , WP.Var 897 "NOP_897"
  , WP.Seq 897 898
  , WP.Var 898 "IF_ELSE_FOOTER"
  , WP.Branch 899 (Eq (Num 0) (Num 1)) 900 906
  , WP.Var 900 "tracer"
  , WP.Seq 900 901
  , WP.Branch 901 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 903 904
  , WP.Var 903 "data"
  , WP.Seq 903 904
  , WP.Seq 903 905
  , WP.Var 904 "NOP_904"
  , WP.Seq 904 905
  , WP.Var 905 "IF_ELSE_FOOTER"
  , WP.Seq 905 899
  , WP.Var 906 "LOOP_FOOTER"
  , WP.Seq 906 907
  , WP.Var 907 "dealloc"
  , WP.Seq 907 908
  , WP.Seq 907 909
  , WP.Var 908 "NOP_908"
  , WP.Seq 908 909
  , WP.Var 909 "IF_ELSE_FOOTER"
  , WP.Seq 909 889
  , WP.Var 910 "LOOP_FOOTER"
  , WP.Seq 910 911
  , WP.Assign 911 "stack_pointer" (Num 0)
  , WP.Seq 911 912
  , WP.Assign 912 "stack_pointer" (Num 0)
  , WP.Seq 912 913
  , WP.Var 913 "IF_ELSE_FOOTER"
  , WP.Assign 914 "stack_pointer" (Num 0)
  , WP.Seq 914 915
  , WP.Assign 915 "stack_pointer" (Num 0)
  , WP.Seq 915 916
  , WP.Branch 916 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 918 918
  , WP.Seq 917 1817
  , WP.Seq 917 919
  , WP.Var 918 "NOP_918"
  , WP.Seq 918 919
  , WP.Var 919 "IF_ELSE_FOOTER"
  , WP.Assign 920 "res" (Num 0)
  , WP.Seq 920 921
  , WP.Assign 921 "undefed" (Num 0)
  , WP.Seq 921 922
  , WP.Assign 922 "stack_pointer" (Num 0)
  , WP.Seq 922 923
  , WP.Var 923 "word"
  , WP.Seq 923 924
  , WP.Assign 924 "opcode" (Num 0)
  , WP.Seq 924 925
  , WP.Assign 925 "oparg" (Num 0)
  , WP.Seq 925 926
  , WP.Branch 926 (Eq (Num 0) (Num 1)) 927 930
  , WP.Var 927 "word"
  , WP.Seq 927 928
  , WP.Assign 928 "opcode" (Num 0)
  , WP.Seq 928 929
  , WP.Assign 929 "oparg" (Num 0)
  , WP.Seq 929 930
  , WP.Seq 929 926
  , WP.Var 930 "LOOP_FOOTER"
  , WP.Seq 930 931
  , WP.Seq 930 35
  , WP.Branch 931 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 933 980
  , WP.Var 933 "NOP_933"
  , WP.Var 934 "__CLABEL_TARGET_BUILD_INTERPOLATION"
  , WP.Seq 934 935
  , WP.Assign 935 "undefed" (Num 0)
  , WP.Seq 935 936
  , WP.Assign 936 "next_instr" (Num 0)
  , WP.Seq 936 937
  , WP.Var 937 "value"
  , WP.Seq 937 938
  , WP.Var 938 "str"
  , WP.Seq 938 939
  , WP.Var 939 "format"
  , WP.Seq 939 940
  , WP.Var 940 "interpolation"
  , WP.Seq 940 941
  , WP.Assign 941 "format" (Num 0)
  , WP.Seq 941 942
  , WP.Assign 942 "str" (Num 0)
  , WP.Seq 942 943
  , WP.Assign 943 "value" (Num 0)
  , WP.Seq 943 944
  , WP.Var 944 "value_o"
  , WP.Seq 944 945
  , WP.Var 945 "str_o"
  , WP.Seq 945 946
  , WP.Var 946 "conversion"
  , WP.Seq 946 947
  , WP.Var 947 "format_o"
  , WP.Seq 947 948
  , WP.Branch 948 (Eq (Plus (Id "oparg") (Num 0)) (Num 1)) 950 951
  , WP.Assign 950 "format_o" (Num 0)
  , WP.Seq 950 952
  , WP.Assign 951 "format_o" (Num 0)
  , WP.Seq 951 952
  , WP.Var 952 "IF_ELSE_FOOTER"
  , WP.Var 953 "interpolation_o"
  , WP.Seq 953 954
  , WP.Assign 954 "stack_pointer" (Num 0)
  , WP.Seq 954 955
  , WP.Branch 955 (Eq (Plus (Id "oparg") (Num 0)) (Num 1)) 957 959
  , WP.Assign 957 "stack_pointer" (Num 0)
  , WP.Seq 957 958
  , WP.Assign 958 "stack_pointer" (Num 0)
  , WP.Seq 958 959
  , WP.Seq 958 960
  , WP.Assign 959 "stack_pointer" (Num 0)
  , WP.Seq 959 960
  , WP.Var 960 "IF_ELSE_FOOTER"
  , WP.Assign 961 "stack_pointer" (Num 0)
  , WP.Seq 961 962
  , WP.Assign 962 "stack_pointer" (Num 0)
  , WP.Seq 962 963
  , WP.Assign 963 "stack_pointer" (Num 0)
  , WP.Seq 963 964
  , WP.Assign 964 "stack_pointer" (Num 0)
  , WP.Seq 964 965
  , WP.Branch 965 (Eq (Plus (Id "interpolation_o") (Num 0)) (Num 1)) 967 967
  , WP.Seq 966 1817
  , WP.Seq 966 968
  , WP.Var 967 "NOP_967"
  , WP.Seq 967 968
  , WP.Var 968 "IF_ELSE_FOOTER"
  , WP.Assign 969 "interpolation" (Num 0)
  , WP.Seq 969 970
  , WP.Assign 970 "undefed" (Num 0)
  , WP.Seq 970 971
  , WP.Assign 971 "stack_pointer" (Num 0)
  , WP.Seq 971 972
  , WP.Var 972 "word"
  , WP.Seq 972 973
  , WP.Assign 973 "opcode" (Num 0)
  , WP.Seq 973 974
  , WP.Assign 974 "oparg" (Num 0)
  , WP.Seq 974 975
  , WP.Branch 975 (Eq (Num 0) (Num 1)) 976 979
  , WP.Var 976 "word"
  , WP.Seq 976 977
  , WP.Assign 977 "opcode" (Num 0)
  , WP.Seq 977 978
  , WP.Assign 978 "oparg" (Num 0)
  , WP.Seq 978 979
  , WP.Seq 978 975
  , WP.Var 979 "LOOP_FOOTER"
  , WP.Seq 979 980
  , WP.Seq 979 35
  , WP.Branch 980 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 982 1006
  , WP.Var 982 "NOP_982"
  , WP.Var 983 "__CLABEL_TARGET_BUILD_LIST"
  , WP.Seq 983 984
  , WP.Assign 984 "undefed" (Num 0)
  , WP.Seq 984 985
  , WP.Assign 985 "next_instr" (Num 0)
  , WP.Seq 985 986
  , WP.Var 986 "values"
  , WP.Seq 986 987
  , WP.Var 987 "list"
  , WP.Seq 987 988
  , WP.Assign 988 "values" (Num 0)
  , WP.Seq 988 989
  , WP.Var 989 "list_o"
  , WP.Seq 989 990
  , WP.Assign 990 "stack_pointer" (Num 0)
  , WP.Seq 990 991
  , WP.Branch 991 (Eq (Plus (Id "list_o") (Num 0)) (Num 1)) 993 993
  , WP.Seq 992 1817
  , WP.Seq 992 994
  , WP.Var 993 "NOP_993"
  , WP.Seq 993 994
  , WP.Var 994 "IF_ELSE_FOOTER"
  , WP.Assign 995 "list" (Num 0)
  , WP.Seq 995 996
  , WP.Assign 996 "undefed" (Num 0)
  , WP.Seq 996 997
  , WP.Assign 997 "stack_pointer" (Num 0)
  , WP.Seq 997 998
  , WP.Var 998 "word"
  , WP.Seq 998 999
  , WP.Assign 999 "opcode" (Num 0)
  , WP.Seq 999 1000
  , WP.Assign 1000 "oparg" (Num 0)
  , WP.Seq 1000 1001
  , WP.Branch 1001 (Eq (Num 0) (Num 1)) 1002 1005
  , WP.Var 1002 "word"
  , WP.Seq 1002 1003
  , WP.Assign 1003 "opcode" (Num 0)
  , WP.Seq 1003 1004
  , WP.Assign 1004 "oparg" (Num 0)
  , WP.Seq 1004 1005
  , WP.Seq 1004 1001
  , WP.Var 1005 "LOOP_FOOTER"
  , WP.Seq 1005 1006
  , WP.Seq 1005 35
  , WP.Branch 1006 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1008 1054
  , WP.Var 1008 "NOP_1008"
  , WP.Var 1009 "__CLABEL_TARGET_BUILD_MAP"
  , WP.Seq 1009 1010
  , WP.Assign 1010 "undefed" (Num 0)
  , WP.Seq 1010 1011
  , WP.Assign 1011 "next_instr" (Num 0)
  , WP.Seq 1011 1012
  , WP.Var 1012 "values"
  , WP.Seq 1012 1013
  , WP.Var 1013 "map"
  , WP.Seq 1013 1014
  , WP.Assign 1014 "values" (Num 0)
  , WP.Seq 1014 1015
  , WP.Var 1015 "values_o_temp"
  , WP.Seq 1015 1016
  , WP.Var 1016 "values_o"
  , WP.Seq 1016 1017
  , WP.Branch 1017 (Eq (Plus (Id "values_o") (Num 0)) (Num 1)) 1019 1027
  , WP.Var 1019 "tmp"
  , WP.Seq 1019 1020
  , WP.Var 1020 "_i"
  , WP.Seq 1020 1021
  , WP.Branch 1021 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1022 1024
  , WP.Assign 1022 "tmp" (Num 0)
  , WP.Seq 1022 1023
  , WP.Assign 1023 "undefed" (Num 0)
  , WP.Seq 1023 1024
  , WP.Seq 1023 1021
  , WP.Var 1024 "LOOP_FOOTER"
  , WP.Seq 1024 1025
  , WP.Assign 1025 "stack_pointer" (Num 0)
  , WP.Seq 1025 1026
  , WP.Assign 1026 "stack_pointer" (Num 0)
  , WP.Seq 1026 1027
  , WP.Seq 1026 1817
  , WP.Seq 1026 1028
  , WP.Var 1027 "NOP_1027"
  , WP.Seq 1027 1028
  , WP.Var 1028 "IF_ELSE_FOOTER"
  , WP.Var 1029 "map_o"
  , WP.Seq 1029 1030
  , WP.Assign 1030 "stack_pointer" (Num 0)
  , WP.Seq 1030 1031
  , WP.Var 1031 "tmp"
  , WP.Seq 1031 1032
  , WP.Var 1032 "_i"
  , WP.Seq 1032 1033
  , WP.Branch 1033 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1034 1036
  , WP.Assign 1034 "tmp" (Num 0)
  , WP.Seq 1034 1035
  , WP.Assign 1035 "undefed" (Num 0)
  , WP.Seq 1035 1036
  , WP.Seq 1035 1033
  , WP.Var 1036 "LOOP_FOOTER"
  , WP.Seq 1036 1037
  , WP.Assign 1037 "stack_pointer" (Num 0)
  , WP.Seq 1037 1038
  , WP.Assign 1038 "stack_pointer" (Num 0)
  , WP.Seq 1038 1039
  , WP.Branch 1039 (Eq (Plus (Id "map_o") (Num 0)) (Num 1)) 1041 1041
  , WP.Seq 1040 1817
  , WP.Seq 1040 1042
  , WP.Var 1041 "NOP_1041"
  , WP.Seq 1041 1042
  , WP.Var 1042 "IF_ELSE_FOOTER"
  , WP.Assign 1043 "map" (Num 0)
  , WP.Seq 1043 1044
  , WP.Assign 1044 "undefed" (Num 0)
  , WP.Seq 1044 1045
  , WP.Assign 1045 "stack_pointer" (Num 0)
  , WP.Seq 1045 1046
  , WP.Var 1046 "word"
  , WP.Seq 1046 1047
  , WP.Assign 1047 "opcode" (Num 0)
  , WP.Seq 1047 1048
  , WP.Assign 1048 "oparg" (Num 0)
  , WP.Seq 1048 1049
  , WP.Branch 1049 (Eq (Num 0) (Num 1)) 1050 1053
  , WP.Var 1050 "word"
  , WP.Seq 1050 1051
  , WP.Assign 1051 "opcode" (Num 0)
  , WP.Seq 1051 1052
  , WP.Assign 1052 "oparg" (Num 0)
  , WP.Seq 1052 1053
  , WP.Seq 1052 1049
  , WP.Var 1053 "LOOP_FOOTER"
  , WP.Seq 1053 1054
  , WP.Seq 1053 35
  , WP.Branch 1054 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1056 1148
  , WP.Var 1056 "NOP_1056"
  , WP.Var 1057 "__CLABEL_TARGET_BUILD_SET"
  , WP.Seq 1057 1058
  , WP.Assign 1058 "undefed" (Num 0)
  , WP.Seq 1058 1059
  , WP.Assign 1059 "next_instr" (Num 0)
  , WP.Seq 1059 1060
  , WP.Var 1060 "values"
  , WP.Seq 1060 1061
  , WP.Var 1061 "set"
  , WP.Seq 1061 1062
  , WP.Assign 1062 "values" (Num 0)
  , WP.Seq 1062 1063
  , WP.Var 1063 "set_o"
  , WP.Seq 1063 1064
  , WP.Assign 1064 "stack_pointer" (Num 0)
  , WP.Seq 1064 1065
  , WP.Branch 1065 (Eq (Plus (Id "set_o") (Num 0)) (Num 1)) 1067 1075
  , WP.Var 1067 "tmp"
  , WP.Seq 1067 1068
  , WP.Var 1068 "_i"
  , WP.Seq 1068 1069
  , WP.Branch 1069 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1070 1072
  , WP.Assign 1070 "tmp" (Num 0)
  , WP.Seq 1070 1071
  , WP.Assign 1071 "undefed" (Num 0)
  , WP.Seq 1071 1072
  , WP.Seq 1071 1069
  , WP.Var 1072 "LOOP_FOOTER"
  , WP.Seq 1072 1073
  , WP.Assign 1073 "stack_pointer" (Num 0)
  , WP.Seq 1073 1074
  , WP.Assign 1074 "stack_pointer" (Num 0)
  , WP.Seq 1074 1075
  , WP.Seq 1074 1817
  , WP.Seq 1074 1076
  , WP.Var 1075 "NOP_1075"
  , WP.Seq 1075 1076
  , WP.Var 1076 "IF_ELSE_FOOTER"
  , WP.Var 1077 "err"
  , WP.Seq 1077 1078
  , WP.Var 1078 "i"
  , WP.Seq 1078 1079
  , WP.Branch 1079 (Eq (Plus (Id "i") (Id "oparg")) (Num 1)) 1080 1088
  , WP.Var 1080 "value"
  , WP.Seq 1080 1081
  , WP.Assign 1081 "undefed" (Num 0)
  , WP.Seq 1081 1082
  , WP.Branch 1082 (Eq (Plus (Id "err") (Num 0)) (Num 1)) 1084 1086
  , WP.Assign 1084 "err" (Num 0)
  , WP.Seq 1084 1085
  , WP.Assign 1085 "stack_pointer" (Num 0)
  , WP.Seq 1085 1086
  , WP.Seq 1085 1087
  , WP.Assign 1086 "stack_pointer" (Num 0)
  , WP.Seq 1086 1087
  , WP.Var 1087 "IF_ELSE_FOOTER"
  , WP.Seq 1087 1079
  , WP.Var 1088 "LOOP_FOOTER"
  , WP.Seq 1088 1089
  , WP.Branch 1089 (Eq (Id "err") (Num 1)) 1091 1135
  , WP.Assign 1091 "stack_pointer" (Num 0)
  , WP.Seq 1091 1092
  , WP.Var 1092 "op"
  , WP.Seq 1092 1093
  , WP.Branch 1093 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1095 1110
  , WP.Var 1095 "tracer"
  , WP.Seq 1095 1096
  , WP.Branch 1096 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1098 1099
  , WP.Var 1098 "data"
  , WP.Seq 1098 1099
  , WP.Seq 1098 1100
  , WP.Var 1099 "NOP_1099"
  , WP.Seq 1099 1100
  , WP.Var 1100 "IF_ELSE_FOOTER"
  , WP.Branch 1101 (Eq (Num 0) (Num 1)) 1102 1108
  , WP.Var 1102 "tracer"
  , WP.Seq 1102 1103
  , WP.Branch 1103 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1105 1106
  , WP.Var 1105 "data"
  , WP.Seq 1105 1106
  , WP.Seq 1105 1107
  , WP.Var 1106 "NOP_1106"
  , WP.Seq 1106 1107
  , WP.Var 1107 "IF_ELSE_FOOTER"
  , WP.Seq 1107 1101
  , WP.Var 1108 "LOOP_FOOTER"
  , WP.Seq 1108 1109
  , WP.Var 1109 "dealloc"
  , WP.Seq 1109 1110
  , WP.Seq 1109 1111
  , WP.Var 1110 "NOP_1110"
  , WP.Seq 1110 1111
  , WP.Var 1111 "IF_ELSE_FOOTER"
  , WP.Branch 1112 (Eq (Num 0) (Num 1)) 1113 1133
  , WP.Var 1113 "op"
  , WP.Seq 1113 1114
  , WP.Branch 1114 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1116 1131
  , WP.Var 1116 "tracer"
  , WP.Seq 1116 1117
  , WP.Branch 1117 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1119 1120
  , WP.Var 1119 "data"
  , WP.Seq 1119 1120
  , WP.Seq 1119 1121
  , WP.Var 1120 "NOP_1120"
  , WP.Seq 1120 1121
  , WP.Var 1121 "IF_ELSE_FOOTER"
  , WP.Branch 1122 (Eq (Num 0) (Num 1)) 1123 1129
  , WP.Var 1123 "tracer"
  , WP.Seq 1123 1124
  , WP.Branch 1124 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1126 1127
  , WP.Var 1126 "data"
  , WP.Seq 1126 1127
  , WP.Seq 1126 1128
  , WP.Var 1127 "NOP_1127"
  , WP.Seq 1127 1128
  , WP.Var 1128 "IF_ELSE_FOOTER"
  , WP.Seq 1128 1122
  , WP.Var 1129 "LOOP_FOOTER"
  , WP.Seq 1129 1130
  , WP.Var 1130 "dealloc"
  , WP.Seq 1130 1131
  , WP.Seq 1130 1132
  , WP.Var 1131 "NOP_1131"
  , WP.Seq 1131 1132
  , WP.Var 1132 "IF_ELSE_FOOTER"
  , WP.Seq 1132 1112
  , WP.Var 1133 "LOOP_FOOTER"
  , WP.Seq 1133 1134
  , WP.Assign 1134 "stack_pointer" (Num 0)
  , WP.Seq 1134 1135
  , WP.Seq 1134 1817
  , WP.Seq 1134 1136
  , WP.Var 1135 "NOP_1135"
  , WP.Seq 1135 1136
  , WP.Var 1136 "IF_ELSE_FOOTER"
  , WP.Assign 1137 "set" (Num 0)
  , WP.Seq 1137 1138
  , WP.Assign 1138 "undefed" (Num 0)
  , WP.Seq 1138 1139
  , WP.Assign 1139 "stack_pointer" (Num 0)
  , WP.Seq 1139 1140
  , WP.Var 1140 "word"
  , WP.Seq 1140 1141
  , WP.Assign 1141 "opcode" (Num 0)
  , WP.Seq 1141 1142
  , WP.Assign 1142 "oparg" (Num 0)
  , WP.Seq 1142 1143
  , WP.Branch 1143 (Eq (Num 0) (Num 1)) 1144 1147
  , WP.Var 1144 "word"
  , WP.Seq 1144 1145
  , WP.Assign 1145 "opcode" (Num 0)
  , WP.Seq 1145 1146
  , WP.Assign 1146 "oparg" (Num 0)
  , WP.Seq 1146 1147
  , WP.Seq 1146 1143
  , WP.Var 1147 "LOOP_FOOTER"
  , WP.Seq 1147 1148
  , WP.Seq 1147 35
  , WP.Branch 1148 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1150 1184
  , WP.Var 1150 "NOP_1150"
  , WP.Var 1151 "__CLABEL_TARGET_BUILD_SLICE"
  , WP.Seq 1151 1152
  , WP.Assign 1152 "undefed" (Num 0)
  , WP.Seq 1152 1153
  , WP.Assign 1153 "next_instr" (Num 0)
  , WP.Seq 1153 1154
  , WP.Var 1154 "args"
  , WP.Seq 1154 1155
  , WP.Var 1155 "slice"
  , WP.Seq 1155 1156
  , WP.Assign 1156 "args" (Num 0)
  , WP.Seq 1156 1157
  , WP.Var 1157 "start_o"
  , WP.Seq 1157 1158
  , WP.Var 1158 "stop_o"
  , WP.Seq 1158 1159
  , WP.Var 1159 "step_o"
  , WP.Seq 1159 1160
  , WP.Var 1160 "slice_o"
  , WP.Seq 1160 1161
  , WP.Var 1161 "tmp"
  , WP.Seq 1161 1162
  , WP.Var 1162 "_i"
  , WP.Seq 1162 1163
  , WP.Branch 1163 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1164 1166
  , WP.Assign 1164 "tmp" (Num 0)
  , WP.Seq 1164 1165
  , WP.Assign 1165 "undefed" (Num 0)
  , WP.Seq 1165 1166
  , WP.Seq 1165 1163
  , WP.Var 1166 "LOOP_FOOTER"
  , WP.Seq 1166 1167
  , WP.Assign 1167 "stack_pointer" (Num 0)
  , WP.Seq 1167 1168
  , WP.Assign 1168 "stack_pointer" (Num 0)
  , WP.Seq 1168 1169
  , WP.Branch 1169 (Eq (Plus (Id "slice_o") (Num 0)) (Num 1)) 1171 1171
  , WP.Seq 1170 1817
  , WP.Seq 1170 1172
  , WP.Var 1171 "NOP_1171"
  , WP.Seq 1171 1172
  , WP.Var 1172 "IF_ELSE_FOOTER"
  , WP.Assign 1173 "slice" (Num 0)
  , WP.Seq 1173 1174
  , WP.Assign 1174 "undefed" (Num 0)
  , WP.Seq 1174 1175
  , WP.Assign 1175 "stack_pointer" (Num 0)
  , WP.Seq 1175 1176
  , WP.Var 1176 "word"
  , WP.Seq 1176 1177
  , WP.Assign 1177 "opcode" (Num 0)
  , WP.Seq 1177 1178
  , WP.Assign 1178 "oparg" (Num 0)
  , WP.Seq 1178 1179
  , WP.Branch 1179 (Eq (Num 0) (Num 1)) 1180 1183
  , WP.Var 1180 "word"
  , WP.Seq 1180 1181
  , WP.Assign 1181 "opcode" (Num 0)
  , WP.Seq 1181 1182
  , WP.Assign 1182 "oparg" (Num 0)
  , WP.Seq 1182 1183
  , WP.Seq 1182 1179
  , WP.Var 1183 "LOOP_FOOTER"
  , WP.Seq 1183 1184
  , WP.Seq 1183 35
  , WP.Branch 1184 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1186 1231
  , WP.Var 1186 "NOP_1186"
  , WP.Var 1187 "__CLABEL_TARGET_BUILD_STRING"
  , WP.Seq 1187 1188
  , WP.Assign 1188 "undefed" (Num 0)
  , WP.Seq 1188 1189
  , WP.Assign 1189 "next_instr" (Num 0)
  , WP.Seq 1189 1190
  , WP.Var 1190 "pieces"
  , WP.Seq 1190 1191
  , WP.Var 1191 "str"
  , WP.Seq 1191 1192
  , WP.Assign 1192 "pieces" (Num 0)
  , WP.Seq 1192 1193
  , WP.Var 1193 "pieces_o_temp"
  , WP.Seq 1193 1194
  , WP.Var 1194 "pieces_o"
  , WP.Seq 1194 1195
  , WP.Branch 1195 (Eq (Plus (Id "pieces_o") (Num 0)) (Num 1)) 1197 1205
  , WP.Var 1197 "tmp"
  , WP.Seq 1197 1198
  , WP.Var 1198 "_i"
  , WP.Seq 1198 1199
  , WP.Branch 1199 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1200 1202
  , WP.Assign 1200 "tmp" (Num 0)
  , WP.Seq 1200 1201
  , WP.Assign 1201 "undefed" (Num 0)
  , WP.Seq 1201 1202
  , WP.Seq 1201 1199
  , WP.Var 1202 "LOOP_FOOTER"
  , WP.Seq 1202 1203
  , WP.Assign 1203 "stack_pointer" (Num 0)
  , WP.Seq 1203 1204
  , WP.Assign 1204 "stack_pointer" (Num 0)
  , WP.Seq 1204 1205
  , WP.Seq 1204 1817
  , WP.Seq 1204 1206
  , WP.Var 1205 "NOP_1205"
  , WP.Seq 1205 1206
  , WP.Var 1206 "IF_ELSE_FOOTER"
  , WP.Var 1207 "str_o"
  , WP.Seq 1207 1208
  , WP.Var 1208 "tmp"
  , WP.Seq 1208 1209
  , WP.Var 1209 "_i"
  , WP.Seq 1209 1210
  , WP.Branch 1210 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1211 1213
  , WP.Assign 1211 "tmp" (Num 0)
  , WP.Seq 1211 1212
  , WP.Assign 1212 "undefed" (Num 0)
  , WP.Seq 1212 1213
  , WP.Seq 1212 1210
  , WP.Var 1213 "LOOP_FOOTER"
  , WP.Seq 1213 1214
  , WP.Assign 1214 "stack_pointer" (Num 0)
  , WP.Seq 1214 1215
  , WP.Assign 1215 "stack_pointer" (Num 0)
  , WP.Seq 1215 1216
  , WP.Branch 1216 (Eq (Plus (Id "str_o") (Num 0)) (Num 1)) 1218 1218
  , WP.Seq 1217 1817
  , WP.Seq 1217 1219
  , WP.Var 1218 "NOP_1218"
  , WP.Seq 1218 1219
  , WP.Var 1219 "IF_ELSE_FOOTER"
  , WP.Assign 1220 "str" (Num 0)
  , WP.Seq 1220 1221
  , WP.Assign 1221 "undefed" (Num 0)
  , WP.Seq 1221 1222
  , WP.Assign 1222 "stack_pointer" (Num 0)
  , WP.Seq 1222 1223
  , WP.Var 1223 "word"
  , WP.Seq 1223 1224
  , WP.Assign 1224 "opcode" (Num 0)
  , WP.Seq 1224 1225
  , WP.Assign 1225 "oparg" (Num 0)
  , WP.Seq 1225 1226
  , WP.Branch 1226 (Eq (Num 0) (Num 1)) 1227 1230
  , WP.Var 1227 "word"
  , WP.Seq 1227 1228
  , WP.Assign 1228 "opcode" (Num 0)
  , WP.Seq 1228 1229
  , WP.Assign 1229 "oparg" (Num 0)
  , WP.Seq 1229 1230
  , WP.Seq 1229 1226
  , WP.Var 1230 "LOOP_FOOTER"
  , WP.Seq 1230 1231
  , WP.Seq 1230 35
  , WP.Branch 1231 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1233 1265
  , WP.Var 1233 "NOP_1233"
  , WP.Var 1234 "__CLABEL_TARGET_BUILD_TEMPLATE"
  , WP.Seq 1234 1235
  , WP.Assign 1235 "undefed" (Num 0)
  , WP.Seq 1235 1236
  , WP.Assign 1236 "next_instr" (Num 0)
  , WP.Seq 1236 1237
  , WP.Var 1237 "strings"
  , WP.Seq 1237 1238
  , WP.Var 1238 "interpolations"
  , WP.Seq 1238 1239
  , WP.Var 1239 "template"
  , WP.Seq 1239 1240
  , WP.Assign 1240 "interpolations" (Num 0)
  , WP.Seq 1240 1241
  , WP.Assign 1241 "strings" (Num 0)
  , WP.Seq 1241 1242
  , WP.Var 1242 "strings_o"
  , WP.Seq 1242 1243
  , WP.Var 1243 "interpolations_o"
  , WP.Seq 1243 1244
  , WP.Var 1244 "template_o"
  , WP.Seq 1244 1245
  , WP.Assign 1245 "stack_pointer" (Num 0)
  , WP.Seq 1245 1246
  , WP.Assign 1246 "stack_pointer" (Num 0)
  , WP.Seq 1246 1247
  , WP.Assign 1247 "stack_pointer" (Num 0)
  , WP.Seq 1247 1248
  , WP.Assign 1248 "stack_pointer" (Num 0)
  , WP.Seq 1248 1249
  , WP.Assign 1249 "stack_pointer" (Num 0)
  , WP.Seq 1249 1250
  , WP.Branch 1250 (Eq (Plus (Id "template_o") (Num 0)) (Num 1)) 1252 1252
  , WP.Seq 1251 1817
  , WP.Seq 1251 1253
  , WP.Var 1252 "NOP_1252"
  , WP.Seq 1252 1253
  , WP.Var 1253 "IF_ELSE_FOOTER"
  , WP.Assign 1254 "template" (Num 0)
  , WP.Seq 1254 1255
  , WP.Assign 1255 "undefed" (Num 0)
  , WP.Seq 1255 1256
  , WP.Assign 1256 "stack_pointer" (Num 0)
  , WP.Seq 1256 1257
  , WP.Var 1257 "word"
  , WP.Seq 1257 1258
  , WP.Assign 1258 "opcode" (Num 0)
  , WP.Seq 1258 1259
  , WP.Assign 1259 "oparg" (Num 0)
  , WP.Seq 1259 1260
  , WP.Branch 1260 (Eq (Num 0) (Num 1)) 1261 1264
  , WP.Var 1261 "word"
  , WP.Seq 1261 1262
  , WP.Assign 1262 "opcode" (Num 0)
  , WP.Seq 1262 1263
  , WP.Assign 1263 "oparg" (Num 0)
  , WP.Seq 1263 1264
  , WP.Seq 1263 1260
  , WP.Var 1264 "LOOP_FOOTER"
  , WP.Seq 1264 1265
  , WP.Seq 1264 35
  , WP.Branch 1265 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1267 1290
  , WP.Var 1267 "NOP_1267"
  , WP.Var 1268 "__CLABEL_TARGET_BUILD_TUPLE"
  , WP.Seq 1268 1269
  , WP.Assign 1269 "undefed" (Num 0)
  , WP.Seq 1269 1270
  , WP.Assign 1270 "next_instr" (Num 0)
  , WP.Seq 1270 1271
  , WP.Var 1271 "values"
  , WP.Seq 1271 1272
  , WP.Var 1272 "tup"
  , WP.Seq 1272 1273
  , WP.Assign 1273 "values" (Num 0)
  , WP.Seq 1273 1274
  , WP.Var 1274 "tup_o"
  , WP.Seq 1274 1275
  , WP.Branch 1275 (Eq (Plus (Id "tup_o") (Num 0)) (Num 1)) 1277 1277
  , WP.Seq 1276 1817
  , WP.Seq 1276 1278
  , WP.Var 1277 "NOP_1277"
  , WP.Seq 1277 1278
  , WP.Var 1278 "IF_ELSE_FOOTER"
  , WP.Assign 1279 "tup" (Num 0)
  , WP.Seq 1279 1280
  , WP.Assign 1280 "undefed" (Num 0)
  , WP.Seq 1280 1281
  , WP.Assign 1281 "stack_pointer" (Num 0)
  , WP.Seq 1281 1282
  , WP.Var 1282 "word"
  , WP.Seq 1282 1283
  , WP.Assign 1283 "opcode" (Num 0)
  , WP.Seq 1283 1284
  , WP.Assign 1284 "oparg" (Num 0)
  , WP.Seq 1284 1285
  , WP.Branch 1285 (Eq (Num 0) (Num 1)) 1286 1289
  , WP.Var 1286 "word"
  , WP.Seq 1286 1287
  , WP.Assign 1287 "opcode" (Num 0)
  , WP.Seq 1287 1288
  , WP.Assign 1288 "oparg" (Num 0)
  , WP.Seq 1288 1289
  , WP.Seq 1288 1285
  , WP.Var 1289 "LOOP_FOOTER"
  , WP.Seq 1289 1290
  , WP.Seq 1289 35
  , WP.Branch 1290 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1292 1304
  , WP.Var 1292 "NOP_1292"
  , WP.Var 1293 "__CLABEL_TARGET_CACHE"
  , WP.Seq 1293 1294
  , WP.Assign 1294 "undefed" (Num 0)
  , WP.Seq 1294 1295
  , WP.Assign 1295 "next_instr" (Num 0)
  , WP.Seq 1295 1296
  , WP.Var 1296 "word"
  , WP.Seq 1296 1297
  , WP.Assign 1297 "opcode" (Num 0)
  , WP.Seq 1297 1298
  , WP.Assign 1298 "oparg" (Num 0)
  , WP.Seq 1298 1299
  , WP.Branch 1299 (Eq (Num 0) (Num 1)) 1300 1303
  , WP.Var 1300 "word"
  , WP.Seq 1300 1301
  , WP.Assign 1301 "opcode" (Num 0)
  , WP.Seq 1301 1302
  , WP.Assign 1302 "oparg" (Num 0)
  , WP.Seq 1302 1303
  , WP.Seq 1302 1299
  , WP.Var 1303 "LOOP_FOOTER"
  , WP.Seq 1303 1304
  , WP.Seq 1303 35
  , WP.Branch 1304 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1306 1551
  , WP.Var 1306 "NOP_1306"
  , WP.Var 1307 "__CLABEL_TARGET_CALL"
  , WP.Seq 1307 1308
  , WP.Assign 1308 "undefed" (Num 0)
  , WP.Seq 1308 1309
  , WP.Assign 1309 "next_instr" (Num 0)
  , WP.Seq 1309 1310
  , WP.Var 1310 "__CLABEL_PREDICTED_CALL"
  , WP.Seq 1310 1311
  , WP.Var 1311 "NOP_1311"
  , WP.Var 1312 "this_instr"
  , WP.Seq 1312 1313
  , WP.Assign 1313 "opcode" (Num 0)
  , WP.Seq 1313 1314
  , WP.Var 1314 "callable"
  , WP.Seq 1314 1315
  , WP.Var 1315 "self_or_null"
  , WP.Seq 1315 1316
  , WP.Var 1316 "args"
  , WP.Seq 1316 1317
  , WP.Var 1317 "res"
  , WP.Seq 1317 1318
  , WP.Assign 1318 "self_or_null" (Num 0)
  , WP.Seq 1318 1319
  , WP.Assign 1319 "callable" (Num 0)
  , WP.Seq 1319 1320
  , WP.Var 1320 "counter"
  , WP.Seq 1320 1321
  , WP.Branch 1321 (Eq (Num 0) (Num 1)) 1323 1326
  , WP.Assign 1323 "next_instr" (Num 0)
  , WP.Seq 1323 1324
  , WP.Assign 1324 "stack_pointer" (Num 0)
  , WP.Seq 1324 1325
  , WP.Assign 1325 "opcode" (Num 0)
  , WP.Seq 1325 1326
  , WP.Seq 1325 35
  , WP.Seq 1325 1327
  , WP.Var 1326 "NOP_1326"
  , WP.Seq 1326 1327
  , WP.Var 1327 "IF_ELSE_FOOTER"
  , WP.Assign 1328 "undefed" (Num 0)
  , WP.Seq 1328 1329
  , WP.Branch 1329 (Eq (Num 0) (Num 1)) 1330 1331
  , WP.Assign 1330 "undefed" (Num 0)
  , WP.Seq 1330 1329
  , WP.Var 1331 "LOOP_FOOTER"
  , WP.Seq 1331 1332
  , WP.Branch 1332 (Eq (Plus (Plus (Num 0) (Plus (Id "PyMethod_Type") (Num 0))) (Plus (Num 0) (Num 0))) (Num 1)) 1334 1343
  , WP.Var 1334 "callable_o"
  , WP.Seq 1334 1335
  , WP.Var 1335 "self"
  , WP.Seq 1335 1336
  , WP.Assign 1336 "self_or_null" (Num 0)
  , WP.Seq 1336 1337
  , WP.Var 1337 "method"
  , WP.Seq 1337 1338
  , WP.Var 1338 "temp"
  , WP.Seq 1338 1339
  , WP.Assign 1339 "callable" (Num 0)
  , WP.Seq 1339 1340
  , WP.Assign 1340 "undefed" (Num 0)
  , WP.Seq 1340 1341
  , WP.Assign 1341 "undefed" (Num 0)
  , WP.Seq 1341 1342
  , WP.Assign 1342 "stack_pointer" (Num 0)
  , WP.Seq 1342 1343
  , WP.Seq 1342 1344
  , WP.Var 1343 "NOP_1343"
  , WP.Seq 1343 1344
  , WP.Var 1344 "IF_ELSE_FOOTER"
  , WP.Assign 1345 "args" (Num 0)
  , WP.Seq 1345 1346
  , WP.Var 1346 "callable_o"
  , WP.Seq 1346 1347
  , WP.Var 1347 "total_args"
  , WP.Seq 1347 1348
  , WP.Var 1348 "arguments"
  , WP.Seq 1348 1349
  , WP.Branch 1349 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1351 1352
  , WP.Assign 1351 "total_args" (Num 0)
  , WP.Seq 1351 1352
  , WP.Seq 1351 1353
  , WP.Var 1352 "NOP_1352"
  , WP.Seq 1352 1353
  , WP.Var 1353 "IF_ELSE_FOOTER"
  , WP.Branch 1354 (Eq (Plus (Plus (Plus (Num 0) (Plus (Id "PyFunction_Type") (Num 0))) (Plus (Num 0) (Num 0))) (Plus (Num 0) (Id "_PyFunction_Vectorcall"))) (Num 1)) 1356 1372
  , WP.Var 1356 "code_flags"
  , WP.Seq 1356 1357
  , WP.Var 1357 "locals"
  , WP.Seq 1357 1358
  , WP.Assign 1358 "undefed" (Num 0)
  , WP.Seq 1358 1359
  , WP.Assign 1359 "undefed" (Num 0)
  , WP.Seq 1359 1360
  , WP.Var 1360 "new_frame"
  , WP.Seq 1360 1361
  , WP.Assign 1361 "stack_pointer" (Num 0)
  , WP.Seq 1361 1362
  , WP.Assign 1362 "stack_pointer" (Num 0)
  , WP.Seq 1362 1363
  , WP.Branch 1363 (Eq (Plus (Id "new_frame") (Num 0)) (Num 1)) 1365 1365
  , WP.Seq 1364 1817
  , WP.Seq 1364 1366
  , WP.Var 1365 "NOP_1365"
  , WP.Seq 1365 1366
  , WP.Var 1366 "IF_ELSE_FOOTER"
  , WP.Assign 1367 "undefed" (Num 0)
  , WP.Seq 1367 1368
  , WP.Assign 1368 "frame" (Num 0)
  , WP.Seq 1368 1369
  , WP.Seq 1368 1886
  , WP.Branch 1369 (Eq (Num 0) (Num 1)) 1370 1371
  , WP.Assign 1370 "frame" (Num 0)
  , WP.Seq 1370 1371
  , WP.Seq 1370 1886
  , WP.Seq 1370 1369
  , WP.Var 1371 "LOOP_FOOTER"
  , WP.Seq 1371 1372
  , WP.Seq 1371 1373
  , WP.Var 1372 "NOP_1372"
  , WP.Seq 1372 1373
  , WP.Var 1373 "IF_ELSE_FOOTER"
  , WP.Var 1374 "args_o_temp"
  , WP.Seq 1374 1375
  , WP.Var 1375 "args_o"
  , WP.Seq 1375 1376
  , WP.Branch 1376 (Eq (Plus (Id "args_o") (Num 0)) (Num 1)) 1378 1394
  , WP.Var 1378 "tmp"
  , WP.Seq 1378 1379
  , WP.Var 1379 "_i"
  , WP.Seq 1379 1380
  , WP.Branch 1380 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1381 1385
  , WP.Assign 1381 "tmp" (Num 0)
  , WP.Seq 1381 1382
  , WP.Assign 1382 "undefed" (Num 0)
  , WP.Seq 1382 1383
  , WP.Assign 1383 "undefed" (Num 0)
  , WP.Seq 1383 1384
  , WP.Assign 1384 "undefed" (Num 0)
  , WP.Seq 1384 1385
  , WP.Seq 1384 1380
  , WP.Var 1385 "LOOP_FOOTER"
  , WP.Seq 1385 1386
  , WP.Assign 1386 "tmp" (Num 0)
  , WP.Seq 1386 1387
  , WP.Assign 1387 "self_or_null" (Num 0)
  , WP.Seq 1387 1388
  , WP.Assign 1388 "undefed" (Num 0)
  , WP.Seq 1388 1389
  , WP.Assign 1389 "tmp" (Num 0)
  , WP.Seq 1389 1390
  , WP.Assign 1390 "callable" (Num 0)
  , WP.Seq 1390 1391
  , WP.Assign 1391 "undefed" (Num 0)
  , WP.Seq 1391 1392
  , WP.Assign 1392 "stack_pointer" (Num 0)
  , WP.Seq 1392 1393
  , WP.Assign 1393 "stack_pointer" (Num 0)
  , WP.Seq 1393 1394
  , WP.Seq 1393 1817
  , WP.Seq 1393 1395
  , WP.Var 1394 "NOP_1394"
  , WP.Seq 1394 1395
  , WP.Var 1395 "IF_ELSE_FOOTER"
  , WP.Assign 1396 "undefed" (Num 0)
  , WP.Seq 1396 1397
  , WP.Assign 1397 "undefed" (Num 0)
  , WP.Seq 1397 1398
  , WP.Var 1398 "res_o"
  , WP.Seq 1398 1399
  , WP.Assign 1399 "stack_pointer" (Num 0)
  , WP.Seq 1399 1400
  , WP.Branch 1400 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1402 1514
  , WP.Var 1402 "arg"
  , WP.Seq 1402 1403
  , WP.Branch 1403 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 1405 1406
  , WP.Assign 1405 "stack_pointer" (Num 0)
  , WP.Seq 1405 1513
  , WP.Var 1406 "err"
  , WP.Seq 1406 1407
  , WP.Assign 1407 "stack_pointer" (Num 0)
  , WP.Seq 1407 1408
  , WP.Branch 1408 (Eq (Plus (Id "err") (Num 0)) (Num 1)) 1410 1511
  , WP.Assign 1410 "undefed" (Num 0)
  , WP.Seq 1410 1411
  , WP.Assign 1411 "_tmp_old_op" (Num 0)
  , WP.Seq 1411 1412
  , WP.Branch 1412 (Eq (Plus (Id "_tmp_old_op") (Num 0)) (Num 1)) 1414 1457
  , WP.Assign 1414 "undefed" (Num 0)
  , WP.Seq 1414 1415
  , WP.Var 1415 "op"
  , WP.Seq 1415 1416
  , WP.Branch 1416 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1418 1433
  , WP.Var 1418 "tracer"
  , WP.Seq 1418 1419
  , WP.Branch 1419 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1421 1422
  , WP.Var 1421 "data"
  , WP.Seq 1421 1422
  , WP.Seq 1421 1423
  , WP.Var 1422 "NOP_1422"
  , WP.Seq 1422 1423
  , WP.Var 1423 "IF_ELSE_FOOTER"
  , WP.Branch 1424 (Eq (Num 0) (Num 1)) 1425 1431
  , WP.Var 1425 "tracer"
  , WP.Seq 1425 1426
  , WP.Branch 1426 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1428 1429
  , WP.Var 1428 "data"
  , WP.Seq 1428 1429
  , WP.Seq 1428 1430
  , WP.Var 1429 "NOP_1429"
  , WP.Seq 1429 1430
  , WP.Var 1430 "IF_ELSE_FOOTER"
  , WP.Seq 1430 1424
  , WP.Var 1431 "LOOP_FOOTER"
  , WP.Seq 1431 1432
  , WP.Var 1432 "dealloc"
  , WP.Seq 1432 1433
  , WP.Seq 1432 1434
  , WP.Var 1433 "NOP_1433"
  , WP.Seq 1433 1434
  , WP.Var 1434 "IF_ELSE_FOOTER"
  , WP.Branch 1435 (Eq (Num 0) (Num 1)) 1436 1456
  , WP.Var 1436 "op"
  , WP.Seq 1436 1437
  , WP.Branch 1437 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1439 1454
  , WP.Var 1439 "tracer"
  , WP.Seq 1439 1440
  , WP.Branch 1440 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1442 1443
  , WP.Var 1442 "data"
  , WP.Seq 1442 1443
  , WP.Seq 1442 1444
  , WP.Var 1443 "NOP_1443"
  , WP.Seq 1443 1444
  , WP.Var 1444 "IF_ELSE_FOOTER"
  , WP.Branch 1445 (Eq (Num 0) (Num 1)) 1446 1452
  , WP.Var 1446 "tracer"
  , WP.Seq 1446 1447
  , WP.Branch 1447 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1449 1450
  , WP.Var 1449 "data"
  , WP.Seq 1449 1450
  , WP.Seq 1449 1451
  , WP.Var 1450 "NOP_1450"
  , WP.Seq 1450 1451
  , WP.Var 1451 "IF_ELSE_FOOTER"
  , WP.Seq 1451 1445
  , WP.Var 1452 "LOOP_FOOTER"
  , WP.Seq 1452 1453
  , WP.Var 1453 "dealloc"
  , WP.Seq 1453 1454
  , WP.Seq 1453 1455
  , WP.Var 1454 "NOP_1454"
  , WP.Seq 1454 1455
  , WP.Var 1455 "IF_ELSE_FOOTER"
  , WP.Seq 1455 1435
  , WP.Var 1456 "LOOP_FOOTER"
  , WP.Seq 1456 1457
  , WP.Seq 1456 1458
  , WP.Var 1457 "NOP_1457"
  , WP.Seq 1457 1458
  , WP.Var 1458 "IF_ELSE_FOOTER"
  , WP.Branch 1459 (Eq (Num 0) (Num 1)) 1460 1509
  , WP.Assign 1460 "undefed" (Num 0)
  , WP.Seq 1460 1461
  , WP.Assign 1461 "_tmp_old_op" (Num 0)
  , WP.Seq 1461 1462
  , WP.Branch 1462 (Eq (Plus (Id "_tmp_old_op") (Num 0)) (Num 1)) 1464 1507
  , WP.Assign 1464 "undefed" (Num 0)
  , WP.Seq 1464 1465
  , WP.Var 1465 "op"
  , WP.Seq 1465 1466
  , WP.Branch 1466 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1468 1483
  , WP.Var 1468 "tracer"
  , WP.Seq 1468 1469
  , WP.Branch 1469 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1471 1472
  , WP.Var 1471 "data"
  , WP.Seq 1471 1472
  , WP.Seq 1471 1473
  , WP.Var 1472 "NOP_1472"
  , WP.Seq 1472 1473
  , WP.Var 1473 "IF_ELSE_FOOTER"
  , WP.Branch 1474 (Eq (Num 0) (Num 1)) 1475 1481
  , WP.Var 1475 "tracer"
  , WP.Seq 1475 1476
  , WP.Branch 1476 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1478 1479
  , WP.Var 1478 "data"
  , WP.Seq 1478 1479
  , WP.Seq 1478 1480
  , WP.Var 1479 "NOP_1479"
  , WP.Seq 1479 1480
  , WP.Var 1480 "IF_ELSE_FOOTER"
  , WP.Seq 1480 1474
  , WP.Var 1481 "LOOP_FOOTER"
  , WP.Seq 1481 1482
  , WP.Var 1482 "dealloc"
  , WP.Seq 1482 1483
  , WP.Seq 1482 1484
  , WP.Var 1483 "NOP_1483"
  , WP.Seq 1483 1484
  , WP.Var 1484 "IF_ELSE_FOOTER"
  , WP.Branch 1485 (Eq (Num 0) (Num 1)) 1486 1506
  , WP.Var 1486 "op"
  , WP.Seq 1486 1487
  , WP.Branch 1487 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1489 1504
  , WP.Var 1489 "tracer"
  , WP.Seq 1489 1490
  , WP.Branch 1490 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1492 1493
  , WP.Var 1492 "data"
  , WP.Seq 1492 1493
  , WP.Seq 1492 1494
  , WP.Var 1493 "NOP_1493"
  , WP.Seq 1493 1494
  , WP.Var 1494 "IF_ELSE_FOOTER"
  , WP.Branch 1495 (Eq (Num 0) (Num 1)) 1496 1502
  , WP.Var 1496 "tracer"
  , WP.Seq 1496 1497
  , WP.Branch 1497 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1499 1500
  , WP.Var 1499 "data"
  , WP.Seq 1499 1500
  , WP.Seq 1499 1501
  , WP.Var 1500 "NOP_1500"
  , WP.Seq 1500 1501
  , WP.Var 1501 "IF_ELSE_FOOTER"
  , WP.Seq 1501 1495
  , WP.Var 1502 "LOOP_FOOTER"
  , WP.Seq 1502 1503
  , WP.Var 1503 "dealloc"
  , WP.Seq 1503 1504
  , WP.Seq 1503 1505
  , WP.Var 1504 "NOP_1504"
  , WP.Seq 1504 1505
  , WP.Var 1505 "IF_ELSE_FOOTER"
  , WP.Seq 1505 1485
  , WP.Var 1506 "LOOP_FOOTER"
  , WP.Seq 1506 1507
  , WP.Seq 1506 1508
  , WP.Var 1507 "NOP_1507"
  , WP.Seq 1507 1508
  , WP.Var 1508 "IF_ELSE_FOOTER"
  , WP.Seq 1508 1459
  , WP.Var 1509 "LOOP_FOOTER"
  , WP.Seq 1509 1510
  , WP.Assign 1510 "stack_pointer" (Num 0)
  , WP.Seq 1510 1511
  , WP.Seq 1510 1512
  , WP.Var 1511 "NOP_1511"
  , WP.Seq 1511 1512
  , WP.Var 1512 "IF_ELSE_FOOTER"
  , WP.Var 1513 "IF_ELSE_FOOTER"
  , WP.Seq 1513 1515
  , WP.Var 1514 "NOP_1514"
  , WP.Seq 1514 1515
  , WP.Var 1515 "IF_ELSE_FOOTER"
  , WP.Var 1516 "tmp"
  , WP.Seq 1516 1517
  , WP.Var 1517 "_i"
  , WP.Seq 1517 1518
  , WP.Branch 1518 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1519 1521
  , WP.Assign 1519 "tmp" (Num 0)
  , WP.Seq 1519 1520
  , WP.Assign 1520 "undefed" (Num 0)
  , WP.Seq 1520 1521
  , WP.Seq 1520 1518
  , WP.Var 1521 "LOOP_FOOTER"
  , WP.Seq 1521 1522
  , WP.Assign 1522 "tmp" (Num 0)
  , WP.Seq 1522 1523
  , WP.Assign 1523 "self_or_null" (Num 0)
  , WP.Seq 1523 1524
  , WP.Assign 1524 "undefed" (Num 0)
  , WP.Seq 1524 1525
  , WP.Assign 1525 "tmp" (Num 0)
  , WP.Seq 1525 1526
  , WP.Assign 1526 "callable" (Num 0)
  , WP.Seq 1526 1527
  , WP.Assign 1527 "undefed" (Num 0)
  , WP.Seq 1527 1528
  , WP.Assign 1528 "stack_pointer" (Num 0)
  , WP.Seq 1528 1529
  , WP.Assign 1529 "stack_pointer" (Num 0)
  , WP.Seq 1529 1530
  , WP.Branch 1530 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 1532 1532
  , WP.Seq 1531 1817
  , WP.Seq 1531 1533
  , WP.Var 1532 "NOP_1532"
  , WP.Seq 1532 1533
  , WP.Var 1533 "IF_ELSE_FOOTER"
  , WP.Assign 1534 "res" (Num 0)
  , WP.Seq 1534 1535
  , WP.Assign 1535 "undefed" (Num 0)
  , WP.Seq 1535 1536
  , WP.Assign 1536 "stack_pointer" (Num 0)
  , WP.Seq 1536 1537
  , WP.Var 1537 "err"
  , WP.Seq 1537 1538
  , WP.Assign 1538 "stack_pointer" (Num 0)
  , WP.Seq 1538 1539
  , WP.Branch 1539 (Eq (Plus (Id "err") (Num 0)) (Num 1)) 1541 1541
  , WP.Seq 1540 1817
  , WP.Seq 1540 1542
  , WP.Var 1541 "NOP_1541"
  , WP.Seq 1541 1542
  , WP.Var 1542 "IF_ELSE_FOOTER"
  , WP.Var 1543 "word"
  , WP.Seq 1543 1544
  , WP.Assign 1544 "opcode" (Num 0)
  , WP.Seq 1544 1545
  , WP.Assign 1545 "oparg" (Num 0)
  , WP.Seq 1545 1546
  , WP.Branch 1546 (Eq (Num 0) (Num 1)) 1547 1550
  , WP.Var 1547 "word"
  , WP.Seq 1547 1548
  , WP.Assign 1548 "opcode" (Num 0)
  , WP.Seq 1548 1549
  , WP.Assign 1549 "oparg" (Num 0)
  , WP.Seq 1549 1550
  , WP.Seq 1549 1546
  , WP.Var 1550 "LOOP_FOOTER"
  , WP.Seq 1550 1551
  , WP.Seq 1550 35
  , WP.Branch 1551 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1553 1637
  , WP.Var 1553 "NOP_1553"
  , WP.Var 1554 "__CLABEL_TARGET_CALL_ALLOC_AND_ENTER_INIT"
  , WP.Seq 1554 1555
  , WP.Var 1555 "this_instr"
  , WP.Seq 1555 1556
  , WP.Assign 1556 "undefed" (Num 0)
  , WP.Seq 1556 1557
  , WP.Assign 1557 "next_instr" (Num 0)
  , WP.Seq 1557 1558
  , WP.Var 1558 "callable"
  , WP.Seq 1558 1559
  , WP.Var 1559 "self_or_null"
  , WP.Seq 1559 1560
  , WP.Var 1560 "init"
  , WP.Seq 1560 1561
  , WP.Var 1561 "self"
  , WP.Seq 1561 1562
  , WP.Var 1562 "args"
  , WP.Seq 1562 1563
  , WP.Var 1563 "init_frame"
  , WP.Seq 1563 1564
  , WP.Var 1564 "new_frame"
  , WP.Seq 1564 1565
  , WP.Branch 1565 (Eq (Num 0) (Num 1)) 1567 1567
  , WP.Seq 1566 1310
  , WP.Seq 1566 1568
  , WP.Var 1567 "NOP_1567"
  , WP.Seq 1567 1568
  , WP.Var 1568 "IF_ELSE_FOOTER"
  , WP.Assign 1569 "self_or_null" (Num 0)
  , WP.Seq 1569 1570
  , WP.Assign 1570 "callable" (Num 0)
  , WP.Seq 1570 1571
  , WP.Var 1571 "type_version"
  , WP.Seq 1571 1572
  , WP.Var 1572 "callable_o"
  , WP.Seq 1572 1573
  , WP.Branch 1573 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1575 1575
  , WP.Seq 1574 1310
  , WP.Seq 1574 1576
  , WP.Var 1575 "NOP_1575"
  , WP.Seq 1575 1576
  , WP.Var 1576 "IF_ELSE_FOOTER"
  , WP.Branch 1577 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1579 1579
  , WP.Seq 1578 1310
  , WP.Seq 1578 1580
  , WP.Var 1579 "NOP_1579"
  , WP.Seq 1579 1580
  , WP.Var 1580 "IF_ELSE_FOOTER"
  , WP.Var 1581 "tp"
  , WP.Seq 1581 1582
  , WP.Branch 1582 (Eq (Plus (Num 0) (Id "type_version")) (Num 1)) 1584 1584
  , WP.Seq 1583 1310
  , WP.Seq 1583 1585
  , WP.Var 1584 "NOP_1584"
  , WP.Seq 1584 1585
  , WP.Var 1585 "IF_ELSE_FOOTER"
  , WP.Var 1586 "cls"
  , WP.Seq 1586 1587
  , WP.Var 1587 "init_func"
  , WP.Seq 1587 1588
  , WP.Var 1588 "code"
  , WP.Seq 1588 1589
  , WP.Branch 1589 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1591 1591
  , WP.Seq 1590 1310
  , WP.Seq 1590 1592
  , WP.Var 1591 "NOP_1591"
  , WP.Seq 1591 1592
  , WP.Var 1592 "IF_ELSE_FOOTER"
  , WP.Var 1593 "self_o"
  , WP.Seq 1593 1594
  , WP.Assign 1594 "stack_pointer" (Num 0)
  , WP.Seq 1594 1595
  , WP.Branch 1595 (Eq (Plus (Id "self_o") (Num 0)) (Num 1)) 1597 1597
  , WP.Seq 1596 1817
  , WP.Seq 1596 1598
  , WP.Var 1597 "NOP_1597"
  , WP.Seq 1597 1598
  , WP.Var 1598 "IF_ELSE_FOOTER"
  , WP.Assign 1599 "self_or_null" (Num 0)
  , WP.Seq 1599 1600
  , WP.Var 1600 "temp"
  , WP.Seq 1600 1601
  , WP.Assign 1601 "callable" (Num 0)
  , WP.Seq 1601 1602
  , WP.Assign 1602 "undefed" (Num 0)
  , WP.Seq 1602 1603
  , WP.Assign 1603 "undefed" (Num 0)
  , WP.Seq 1603 1604
  , WP.Assign 1604 "stack_pointer" (Num 0)
  , WP.Seq 1604 1605
  , WP.Assign 1605 "args" (Num 0)
  , WP.Seq 1605 1606
  , WP.Assign 1606 "self" (Num 0)
  , WP.Seq 1606 1607
  , WP.Assign 1607 "init" (Num 0)
  , WP.Seq 1607 1608
  , WP.Var 1608 "shim"
  , WP.Seq 1608 1609
  , WP.Assign 1609 "stack_pointer" (Num 0)
  , WP.Seq 1609 1610
  , WP.Assign 1610 "undefed" (Num 0)
  , WP.Seq 1610 1611
  , WP.Var 1611 "temp"
  , WP.Seq 1611 1612
  , WP.Assign 1612 "stack_pointer" (Num 0)
  , WP.Seq 1612 1613
  , WP.Assign 1613 "stack_pointer" (Num 0)
  , WP.Seq 1613 1614
  , WP.Branch 1614 (Eq (Plus (Id "temp") (Num 0)) (Num 1)) 1616 1617
  , WP.Assign 1616 "stack_pointer" (Num 0)
  , WP.Seq 1616 1617
  , WP.Seq 1616 1817
  , WP.Seq 1616 1618
  , WP.Var 1617 "NOP_1617"
  , WP.Seq 1617 1618
  , WP.Var 1618 "IF_ELSE_FOOTER"
  , WP.Assign 1619 "undefed" (Num 0)
  , WP.Seq 1619 1620
  , WP.Assign 1620 "init_frame" (Num 0)
  , WP.Seq 1620 1621
  , WP.Assign 1621 "new_frame" (Num 0)
  , WP.Seq 1621 1622
  , WP.Var 1622 "temp"
  , WP.Seq 1622 1623
  , WP.Assign 1623 "frame" (Num 0)
  , WP.Seq 1623 1624
  , WP.Assign 1624 "stack_pointer" (Num 0)
  , WP.Seq 1624 1625
  , WP.Assign 1625 "next_instr" (Num 0)
  , WP.Seq 1625 1626
  , WP.Branch 1626 (Eq (Num 0) (Num 1)) 1627 1628
  , WP.Assign 1627 "next_instr" (Num 0)
  , WP.Seq 1627 1626
  , WP.Var 1628 "LOOP_FOOTER"
  , WP.Seq 1628 1629
  , WP.Var 1629 "word"
  , WP.Seq 1629 1630
  , WP.Assign 1630 "opcode" (Num 0)
  , WP.Seq 1630 1631
  , WP.Assign 1631 "oparg" (Num 0)
  , WP.Seq 1631 1632
  , WP.Branch 1632 (Eq (Num 0) (Num 1)) 1633 1636
  , WP.Var 1633 "word"
  , WP.Seq 1633 1634
  , WP.Assign 1634 "opcode" (Num 0)
  , WP.Seq 1634 1635
  , WP.Assign 1635 "oparg" (Num 0)
  , WP.Seq 1635 1636
  , WP.Seq 1635 1632
  , WP.Var 1636 "LOOP_FOOTER"
  , WP.Seq 1636 1637
  , WP.Seq 1636 35
  , WP.Branch 1637 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1639 1727
  , WP.Var 1639 "NOP_1639"
  , WP.Var 1640 "__CLABEL_TARGET_CALL_BOUND_METHOD_EXACT_ARGS"
  , WP.Seq 1640 1641
  , WP.Var 1641 "this_instr"
  , WP.Seq 1641 1642
  , WP.Assign 1642 "undefed" (Num 0)
  , WP.Seq 1642 1643
  , WP.Assign 1643 "next_instr" (Num 0)
  , WP.Seq 1643 1644
  , WP.Var 1644 "callable"
  , WP.Seq 1644 1645
  , WP.Var 1645 "null"
  , WP.Seq 1645 1646
  , WP.Var 1646 "self_or_null"
  , WP.Seq 1646 1647
  , WP.Var 1647 "args"
  , WP.Seq 1647 1648
  , WP.Var 1648 "new_frame"
  , WP.Seq 1648 1649
  , WP.Branch 1649 (Eq (Num 0) (Num 1)) 1651 1651
  , WP.Seq 1650 1310
  , WP.Seq 1650 1652
  , WP.Var 1651 "NOP_1651"
  , WP.Seq 1651 1652
  , WP.Var 1652 "IF_ELSE_FOOTER"
  , WP.Assign 1653 "null" (Num 0)
  , WP.Seq 1653 1654
  , WP.Assign 1654 "callable" (Num 0)
  , WP.Seq 1654 1655
  , WP.Branch 1655 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1657 1657
  , WP.Seq 1656 1310
  , WP.Seq 1656 1658
  , WP.Var 1657 "NOP_1657"
  , WP.Seq 1657 1658
  , WP.Var 1658 "IF_ELSE_FOOTER"
  , WP.Branch 1659 (Eq (Plus (Num 0) (Plus (Id "PyMethod_Type") (Num 0))) (Num 1)) 1661 1661
  , WP.Seq 1660 1310
  , WP.Seq 1660 1662
  , WP.Var 1661 "NOP_1661"
  , WP.Seq 1661 1662
  , WP.Var 1662 "IF_ELSE_FOOTER"
  , WP.Assign 1663 "self_or_null" (Num 0)
  , WP.Seq 1663 1664
  , WP.Var 1664 "callable_o"
  , WP.Seq 1664 1665
  , WP.Assign 1665 "self_or_null" (Num 0)
  , WP.Seq 1665 1666
  , WP.Var 1666 "temp"
  , WP.Seq 1666 1667
  , WP.Assign 1667 "callable" (Num 0)
  , WP.Seq 1667 1668
  , WP.Assign 1668 "undefed" (Num 0)
  , WP.Seq 1668 1669
  , WP.Assign 1669 "undefed" (Num 0)
  , WP.Seq 1669 1670
  , WP.Assign 1670 "stack_pointer" (Num 0)
  , WP.Seq 1670 1671
  , WP.Var 1671 "func_version"
  , WP.Seq 1671 1672
  , WP.Var 1672 "callable_o"
  , WP.Seq 1672 1673
  , WP.Branch 1673 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFunction_Type") (Num 0))) (Num 0)) (Num 1)) 1675 1675
  , WP.Seq 1674 1310
  , WP.Seq 1674 1676
  , WP.Var 1675 "NOP_1675"
  , WP.Seq 1675 1676
  , WP.Var 1676 "IF_ELSE_FOOTER"
  , WP.Var 1677 "func"
  , WP.Seq 1677 1678
  , WP.Branch 1678 (Eq (Plus (Num 0) (Id "func_version")) (Num 1)) 1680 1680
  , WP.Seq 1679 1310
  , WP.Seq 1679 1681
  , WP.Var 1680 "NOP_1680"
  , WP.Seq 1680 1681
  , WP.Var 1681 "IF_ELSE_FOOTER"
  , WP.Var 1682 "callable_o"
  , WP.Seq 1682 1683
  , WP.Var 1683 "func"
  , WP.Seq 1683 1684
  , WP.Var 1684 "code"
  , WP.Seq 1684 1685
  , WP.Branch 1685 (Eq (Plus (Num 0) (Plus (Id "oparg") (Plus (Plus (Num 0) (Num 0)) (Num 0)))) (Num 1)) 1687 1687
  , WP.Seq 1686 1310
  , WP.Seq 1686 1688
  , WP.Var 1687 "NOP_1687"
  , WP.Seq 1687 1688
  , WP.Var 1688 "IF_ELSE_FOOTER"
  , WP.Var 1689 "callable_o"
  , WP.Seq 1689 1690
  , WP.Var 1690 "func"
  , WP.Seq 1690 1691
  , WP.Var 1691 "code"
  , WP.Seq 1691 1692
  , WP.Branch 1692 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1694 1694
  , WP.Seq 1693 1310
  , WP.Seq 1693 1695
  , WP.Var 1694 "NOP_1694"
  , WP.Seq 1694 1695
  , WP.Var 1695 "IF_ELSE_FOOTER"
  , WP.Branch 1696 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1698 1698
  , WP.Seq 1697 1310
  , WP.Seq 1697 1699
  , WP.Var 1698 "NOP_1698"
  , WP.Seq 1698 1699
  , WP.Var 1699 "IF_ELSE_FOOTER"
  , WP.Assign 1700 "args" (Num 0)
  , WP.Seq 1700 1701
  , WP.Var 1701 "has_self"
  , WP.Seq 1701 1702
  , WP.Var 1702 "pushed_frame"
  , WP.Seq 1702 1703
  , WP.Var 1703 "first_non_self_local"
  , WP.Seq 1703 1704
  , WP.Assign 1704 "undefed" (Num 0)
  , WP.Seq 1704 1705
  , WP.Var 1705 "i"
  , WP.Seq 1705 1706
  , WP.Branch 1706 (Eq (Plus (Id "i") (Id "oparg")) (Num 1)) 1707 1708
  , WP.Assign 1707 "undefed" (Num 0)
  , WP.Seq 1707 1708
  , WP.Seq 1707 1706
  , WP.Var 1708 "LOOP_FOOTER"
  , WP.Seq 1708 1709
  , WP.Assign 1709 "new_frame" (Num 0)
  , WP.Seq 1709 1710
  , WP.Assign 1710 "undefed" (Num 0)
  , WP.Seq 1710 1711
  , WP.Var 1711 "temp"
  , WP.Seq 1711 1712
  , WP.Assign 1712 "stack_pointer" (Num 0)
  , WP.Seq 1712 1713
  , WP.Assign 1713 "frame" (Num 0)
  , WP.Seq 1713 1714
  , WP.Assign 1714 "stack_pointer" (Num 0)
  , WP.Seq 1714 1715
  , WP.Assign 1715 "next_instr" (Num 0)
  , WP.Seq 1715 1716
  , WP.Branch 1716 (Eq (Num 0) (Num 1)) 1717 1718
  , WP.Assign 1717 "next_instr" (Num 0)
  , WP.Seq 1717 1716
  , WP.Var 1718 "LOOP_FOOTER"
  , WP.Seq 1718 1719
  , WP.Var 1719 "word"
  , WP.Seq 1719 1720
  , WP.Assign 1720 "opcode" (Num 0)
  , WP.Seq 1720 1721
  , WP.Assign 1721 "oparg" (Num 0)
  , WP.Seq 1721 1722
  , WP.Branch 1722 (Eq (Num 0) (Num 1)) 1723 1726
  , WP.Var 1723 "word"
  , WP.Seq 1723 1724
  , WP.Assign 1724 "opcode" (Num 0)
  , WP.Seq 1724 1725
  , WP.Assign 1725 "oparg" (Num 0)
  , WP.Seq 1725 1726
  , WP.Seq 1725 1722
  , WP.Var 1726 "LOOP_FOOTER"
  , WP.Seq 1726 1727
  , WP.Seq 1726 35
  , WP.Branch 1727 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1729 1810
  , WP.Var 1729 "NOP_1729"
  , WP.Var 1730 "__CLABEL_TARGET_CALL_BOUND_METHOD_GENERAL"
  , WP.Seq 1730 1731
  , WP.Var 1731 "this_instr"
  , WP.Seq 1731 1732
  , WP.Assign 1732 "undefed" (Num 0)
  , WP.Seq 1732 1733
  , WP.Assign 1733 "next_instr" (Num 0)
  , WP.Seq 1733 1734
  , WP.Var 1734 "callable"
  , WP.Seq 1734 1735
  , WP.Var 1735 "null"
  , WP.Seq 1735 1736
  , WP.Var 1736 "self_or_null"
  , WP.Seq 1736 1737
  , WP.Var 1737 "args"
  , WP.Seq 1737 1738
  , WP.Var 1738 "new_frame"
  , WP.Seq 1738 1739
  , WP.Branch 1739 (Eq (Num 0) (Num 1)) 1741 1741
  , WP.Seq 1740 1310
  , WP.Seq 1740 1742
  , WP.Var 1741 "NOP_1741"
  , WP.Seq 1741 1742
  , WP.Var 1742 "IF_ELSE_FOOTER"
  , WP.Assign 1743 "null" (Num 0)
  , WP.Seq 1743 1744
  , WP.Assign 1744 "callable" (Num 0)
  , WP.Seq 1744 1745
  , WP.Var 1745 "func_version"
  , WP.Seq 1745 1746
  , WP.Var 1746 "callable_o"
  , WP.Seq 1746 1747
  , WP.Branch 1747 (Eq (Plus (Num 0) (Plus (Id "PyMethod_Type") (Num 0))) (Num 1)) 1749 1749
  , WP.Seq 1748 1310
  , WP.Seq 1748 1750
  , WP.Var 1749 "NOP_1749"
  , WP.Seq 1749 1750
  , WP.Var 1750 "IF_ELSE_FOOTER"
  , WP.Var 1751 "func"
  , WP.Seq 1751 1752
  , WP.Branch 1752 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFunction_Type") (Num 0))) (Num 0)) (Num 1)) 1754 1754
  , WP.Seq 1753 1310
  , WP.Seq 1753 1755
  , WP.Var 1754 "NOP_1754"
  , WP.Seq 1754 1755
  , WP.Var 1755 "IF_ELSE_FOOTER"
  , WP.Branch 1756 (Eq (Plus (Num 0) (Id "func_version")) (Num 1)) 1758 1758
  , WP.Seq 1757 1310
  , WP.Seq 1757 1759
  , WP.Var 1758 "NOP_1758"
  , WP.Seq 1758 1759
  , WP.Var 1759 "IF_ELSE_FOOTER"
  , WP.Branch 1760 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1762 1762
  , WP.Seq 1761 1310
  , WP.Seq 1761 1763
  , WP.Var 1762 "NOP_1762"
  , WP.Seq 1762 1763
  , WP.Var 1763 "IF_ELSE_FOOTER"
  , WP.Assign 1764 "self_or_null" (Num 0)
  , WP.Seq 1764 1765
  , WP.Var 1765 "callable_o"
  , WP.Seq 1765 1766
  , WP.Assign 1766 "self_or_null" (Num 0)
  , WP.Seq 1766 1767
  , WP.Var 1767 "temp"
  , WP.Seq 1767 1768
  , WP.Assign 1768 "callable" (Num 0)
  , WP.Seq 1768 1769
  , WP.Assign 1769 "undefed" (Num 0)
  , WP.Seq 1769 1770
  , WP.Assign 1770 "undefed" (Num 0)
  , WP.Seq 1770 1771
  , WP.Assign 1771 "stack_pointer" (Num 0)
  , WP.Seq 1771 1772
  , WP.Branch 1772 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1774 1774
  , WP.Seq 1773 1310
  , WP.Seq 1773 1775
  , WP.Var 1774 "NOP_1774"
  , WP.Seq 1774 1775
  , WP.Var 1775 "IF_ELSE_FOOTER"
  , WP.Assign 1776 "args" (Num 0)
  , WP.Seq 1776 1777
  , WP.Var 1777 "callable_o"
  , WP.Seq 1777 1778
  , WP.Var 1778 "total_args"
  , WP.Seq 1778 1779
  , WP.Branch 1779 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1781 1782
  , WP.Assign 1781 "total_args" (Num 0)
  , WP.Seq 1781 1782
  , WP.Seq 1781 1783
  , WP.Var 1782 "NOP_1782"
  , WP.Seq 1782 1783
  , WP.Var 1783 "IF_ELSE_FOOTER"
  , WP.Var 1784 "code_flags"
  , WP.Seq 1784 1785
  , WP.Var 1785 "locals"
  , WP.Seq 1785 1786
  , WP.Var 1786 "temp"
  , WP.Seq 1786 1787
  , WP.Assign 1787 "stack_pointer" (Num 0)
  , WP.Seq 1787 1788
  , WP.Assign 1788 "stack_pointer" (Num 0)
  , WP.Seq 1788 1789
  , WP.Branch 1789 (Eq (Plus (Id "temp") (Num 0)) (Num 1)) 1791 1791
  , WP.Seq 1790 1817
  , WP.Seq 1790 1792
  , WP.Var 1791 "NOP_1791"
  , WP.Seq 1791 1792
  , WP.Var 1792 "IF_ELSE_FOOTER"
  , WP.Assign 1793 "new_frame" (Num 0)
  , WP.Seq 1793 1794
  , WP.Assign 1794 "undefed" (Num 0)
  , WP.Seq 1794 1795
  , WP.Var 1795 "temp"
  , WP.Seq 1795 1796
  , WP.Assign 1796 "frame" (Num 0)
  , WP.Seq 1796 1797
  , WP.Assign 1797 "stack_pointer" (Num 0)
  , WP.Seq 1797 1798
  , WP.Assign 1798 "next_instr" (Num 0)
  , WP.Seq 1798 1799
  , WP.Branch 1799 (Eq (Num 0) (Num 1)) 1800 1801
  , WP.Assign 1800 "next_instr" (Num 0)
  , WP.Seq 1800 1799
  , WP.Var 1801 "LOOP_FOOTER"
  , WP.Seq 1801 1802
  , WP.Var 1802 "word"
  , WP.Seq 1802 1803
  , WP.Assign 1803 "opcode" (Num 0)
  , WP.Seq 1803 1804
  , WP.Assign 1804 "oparg" (Num 0)
  , WP.Seq 1804 1805
  , WP.Branch 1805 (Eq (Num 0) (Num 1)) 1806 1809
  , WP.Var 1806 "word"
  , WP.Seq 1806 1807
  , WP.Assign 1807 "opcode" (Num 0)
  , WP.Seq 1807 1808
  , WP.Assign 1808 "oparg" (Num 0)
  , WP.Seq 1808 1809
  , WP.Seq 1808 1805
  , WP.Var 1809 "LOOP_FOOTER"
  , WP.Seq 1809 1810
  , WP.Seq 1809 35
  , WP.Var 1810 "NOP_1810"
  , WP.Seq 1810 1811
  , WP.Var 1811 "__CLABEL_CODEGEN_SWITCH_EXIT_0"
  , WP.Seq 1811 1812
  , WP.Var 1812 "NOP_1812"
  , WP.Var 1813 "__CLABEL_pop_2_error"
  , WP.Seq 1813 1814
  , WP.Assign 1814 "stack_pointer" (Num 0)
  , WP.Seq 1814 1815
  , WP.Seq 1814 1817
  , WP.Var 1815 "__CLABEL_pop_1_error"
  , WP.Seq 1815 1816
  , WP.Assign 1816 "stack_pointer" (Num 0)
  , WP.Seq 1816 1817
  , WP.Seq 1816 1817
  , WP.Var 1817 "__CLABEL_error"
  , WP.Seq 1817 1818
  , WP.Branch 1818 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1820 1821
  , WP.Assign 1820 "stack_pointer" (Num 0)
  , WP.Seq 1820 1822
  , WP.Var 1821 "NOP_1821"
  , WP.Seq 1821 1822
  , WP.Var 1822 "IF_ELSE_FOOTER"
  , WP.Branch 1823 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1825 1832
  , WP.Var 1825 "f"
  , WP.Seq 1825 1826
  , WP.Assign 1826 "stack_pointer" (Num 0)
  , WP.Seq 1826 1827
  , WP.Branch 1827 (Eq (Plus (Id "f") (Num 0)) (Num 1)) 1829 1830
  , WP.Assign 1829 "stack_pointer" (Num 0)
  , WP.Seq 1829 1831
  , WP.Var 1830 "NOP_1830"
  , WP.Seq 1830 1831
  , WP.Var 1831 "IF_ELSE_FOOTER"
  , WP.Seq 1831 1833
  , WP.Var 1832 "NOP_1832"
  , WP.Seq 1832 1833
  , WP.Var 1833 "IF_ELSE_FOOTER"
  , WP.Seq 1833 1834
  , WP.Var 1834 "__CLABEL_exception_unwind"
  , WP.Seq 1834 1835
  , WP.Var 1835 "offset"
  , WP.Seq 1835 1836
  , WP.Var 1836 "level"
  , WP.Seq 1836 1837
  , WP.Var 1837 "handler"
  , WP.Seq 1837 1838
  , WP.Var 1838 "lasti"
  , WP.Seq 1838 1839
  , WP.Var 1839 "handled"
  , WP.Seq 1839 1840
  , WP.Branch 1840 (Eq (Plus (Id "handled") (Num 0)) (Num 1)) 1842 1846
  , WP.Var 1842 "stackbase"
  , WP.Seq 1842 1843
  , WP.Branch 1843 (Eq (Plus (Num 0) (Id "stackbase")) (Num 1)) 1844 1845
  , WP.Var 1844 "ref"
  , WP.Seq 1844 1845
  , WP.Seq 1844 1843
  , WP.Var 1845 "LOOP_FOOTER"
  , WP.Seq 1845 1846
  , WP.Seq 1845 1874
  , WP.Seq 1845 1847
  , WP.Var 1846 "NOP_1846"
  , WP.Seq 1846 1847
  , WP.Var 1847 "IF_ELSE_FOOTER"
  , WP.Var 1848 "new_top"
  , WP.Seq 1848 1849
  , WP.Branch 1849 (Eq (Plus (Num 0) (Id "new_top")) (Num 1)) 1850 1851
  , WP.Var 1850 "ref"
  , WP.Seq 1850 1851
  , WP.Seq 1850 1849
  , WP.Var 1851 "LOOP_FOOTER"
  , WP.Seq 1851 1852
  , WP.Branch 1852 (Eq (Id "lasti") (Num 1)) 1854 1856
  , WP.Var 1854 "frame_lasti"
  , WP.Seq 1854 1855
  , WP.Var 1855 "lasti"
  , WP.Seq 1855 1856
  , WP.Seq 1855 1857
  , WP.Var 1856 "NOP_1856"
  , WP.Seq 1856 1857
  , WP.Var 1857 "IF_ELSE_FOOTER"
  , WP.Var 1858 "exc"
  , WP.Seq 1858 1859
  , WP.Assign 1859 "next_instr" (Num 0)
  , WP.Seq 1859 1860
  , WP.Var 1860 "err"
  , WP.Seq 1860 1861
  , WP.Branch 1861 (Eq (Plus (Id "err") (Num 0)) (Num 1)) 1863 1863
  , WP.Seq 1862 1834
  , WP.Seq 1862 1864
  , WP.Var 1863 "NOP_1863"
  , WP.Seq 1863 1864
  , WP.Var 1864 "IF_ELSE_FOOTER"
  , WP.Assign 1865 "stack_pointer" (Num 0)
  , WP.Seq 1865 1866
  , WP.Var 1866 "word"
  , WP.Seq 1866 1867
  , WP.Assign 1867 "opcode" (Num 0)
  , WP.Seq 1867 1868
  , WP.Assign 1868 "oparg" (Num 0)
  , WP.Seq 1868 1869
  , WP.Branch 1869 (Eq (Num 0) (Num 1)) 1870 1873
  , WP.Var 1870 "word"
  , WP.Seq 1870 1871
  , WP.Assign 1871 "opcode" (Num 0)
  , WP.Seq 1871 1872
  , WP.Assign 1872 "oparg" (Num 0)
  , WP.Seq 1872 1873
  , WP.Seq 1872 1869
  , WP.Var 1873 "LOOP_FOOTER"
  , WP.Seq 1873 1874
  , WP.Seq 1873 35
  , WP.Var 1874 "__CLABEL_exit_unwind"
  , WP.Seq 1874 1875
  , WP.Var 1875 "dying"
  , WP.Seq 1875 1876
  , WP.Assign 1876 "frame" (Num 0)
  , WP.Seq 1876 1877
  , WP.Assign 1877 "undefed" (Num 0)
  , WP.Seq 1877 1878
  , WP.Branch 1878 (Eq (Plus (Num 0) (Id "FRAME_OWNED_BY_INTERPRETER")) (Num 1)) 1880 1882
  , WP.Assign 1880 "undefed" (Num 0)
  , WP.Seq 1880 1881
  , WP.Assign 1881 "return" (Num 0)
  , WP.Seq 1881 1882
  , WP.Seq 1881 1883
  , WP.Var 1882 "NOP_1882"
  , WP.Seq 1882 1883
  , WP.Var 1883 "IF_ELSE_FOOTER"
  , WP.Assign 1884 "next_instr" (Num 0)
  , WP.Seq 1884 1885
  , WP.Assign 1885 "stack_pointer" (Num 0)
  , WP.Seq 1885 1886
  , WP.Seq 1885 1817
  , WP.Var 1886 "__CLABEL_start_frame"
  , WP.Seq 1886 1887
  , WP.Var 1887 "too_deep"
  , WP.Seq 1887 1888
  , WP.Branch 1888 (Eq (Id "too_deep") (Num 1)) 1890 1890
  , WP.Seq 1889 1874
  , WP.Seq 1889 1891
  , WP.Var 1890 "NOP_1890"
  , WP.Seq 1890 1891
  , WP.Var 1891 "IF_ELSE_FOOTER"
  , WP.Assign 1892 "next_instr" (Num 0)
  , WP.Seq 1892 1893
  , WP.Assign 1893 "stack_pointer" (Num 0)
  , WP.Seq 1893 1894
  , WP.Var 1894 "word"
  , WP.Seq 1894 1895
  , WP.Assign 1895 "opcode" (Num 0)
  , WP.Seq 1895 1896
  , WP.Assign 1896 "oparg" (Num 0)
  , WP.Seq 1896 1897
  , WP.Branch 1897 (Eq (Num 0) (Num 1)) 1898 1901
  , WP.Var 1898 "word"
  , WP.Seq 1898 1899
  , WP.Assign 1899 "opcode" (Num 0)
  , WP.Seq 1899 1900
  , WP.Assign 1900 "oparg" (Num 0)
  , WP.Seq 1900 1901
  , WP.Seq 1900 1897
  , WP.Var 1901 "LOOP_FOOTER"
  , WP.Seq 1901 1902
  , WP.Seq 1901 35
  , WP.Var 1902 "__CLABEL_early_exit"
  , WP.Seq 1902 1903
  , WP.Var 1903 "NOP_1903"
  , WP.Var 1904 "dying"
  , WP.Seq 1904 1905
  , WP.Assign 1905 "frame" (Num 0)
  , WP.Seq 1905 1906
  , WP.Assign 1906 "undefed" (Num 0)
  , WP.Seq 1906 1907
  , WP.Assign 1907 "undefed" (Num 0)
  , WP.Seq 1907 1908
  , WP.Assign 1908 "return" (Num 0)
  , WP.Seq 1908 1909
  , WP.Seq 1908 1909
  , WP.Var 1909 "PROG_END"
  ]

customTest :: [C.Fact]
customTest =
  [ C.Var 0 "_PyEval_EvalFrameDefault"
  , C.Seq 0 1
  , C.Var 1 "tstate"
  , C.Seq 1 2
  , C.Var 2 "frame"
  , C.Seq 2 3
  , C.Var 3 "throwflag"
  , C.Seq 3 4
  , C.Var 4 "opcode"
  , C.Seq 4 5
  , C.Var 5 "oparg"
  , C.Seq 5 6
  , C.Var 6 "entry"
  , C.Seq 6 7
  , C.Branch 7 (Eq (Num 0) (Num 1)) 9 10
  , C.Assign 9 "return" (Num 0)
  , C.Seq 9 11
  , C.Var 10 "NOP_10"
  , C.Seq 10 11
  , C.Var 11 "IF_ELSE_FOOTER"
  , C.Var 12 "next_instr"
  , C.Seq 12 13
  , C.Var 13 "stack_pointer"
  , C.Seq 13 14
  , C.Assign 14 "undefed" (Num 0)
  , C.Seq 14 15
  , C.Assign 15 "undefed" (Num 0)
  , C.Seq 15 16
  , C.Assign 16 "undefed" (Num 0)
  , C.Seq 16 17
  , C.Assign 17 "undefed" (Num 0)
  , C.Seq 17 18
  , C.Assign 18 "undefed" (Num 0)
  , C.Seq 18 19
  , C.Assign 19 "undefed" (Num 0)
  , C.Seq 19 20
  , C.Assign 20 "undefed" (Num 0)
  , C.Seq 20 21
  , C.Assign 21 "undefed" (Num 0)
  , C.Seq 21 22
  , C.Assign 22 "undefed" (Num 0)
  , C.Seq 22 23
  , C.Assign 23 "undefed" (Num 0)
  , C.Seq 23 24
  , C.Assign 24 "undefed" (Num 0)
  , C.Seq 24 25
  , C.Branch 25 (Eq (Id "throwflag") (Num 1)) 27 33
  , C.Branch 27 (Eq (Num 0) (Num 1)) 29 29
  , C.Seq 28 1902
  , C.Seq 28 30
  , C.Var 29 "NOP_29"
  , C.Seq 29 30
  , C.Var 30 "IF_ELSE_FOOTER"
  , C.Assign 31 "next_instr" (Num 0)
  , C.Seq 31 32
  , C.Assign 32 "stack_pointer" (Num 0)
  , C.Seq 32 33
  , C.Seq 32 1817
  , C.Seq 32 34
  , C.Var 33 "NOP_33"
  , C.Seq 33 34
  , C.Var 34 "IF_ELSE_FOOTER"
  , C.Seq 34 1886
  , C.Var 35 "__CLABEL_dispatch_opcode"
  , C.Seq 35 36
  , C.Branch 36 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 38 87
  , C.Var 38 "NOP_38"
  , C.Var 39 "__CLABEL_TARGET_BINARY_OP"
  , C.Seq 39 40
  , C.Assign 40 "undefed" (Num 0)
  , C.Seq 40 41
  , C.Assign 41 "next_instr" (Num 0)
  , C.Seq 41 42
  , C.Var 42 "__CLABEL_PREDICTED_BINARY_OP"
  , C.Seq 42 43
  , C.Var 43 "NOP_43"
  , C.Var 44 "this_instr"
  , C.Seq 44 45
  , C.Var 45 "lhs"
  , C.Seq 45 46
  , C.Var 46 "rhs"
  , C.Seq 46 47
  , C.Var 47 "res"
  , C.Seq 47 48
  , C.Assign 48 "rhs" (Num 0)
  , C.Seq 48 49
  , C.Assign 49 "lhs" (Num 0)
  , C.Seq 49 50
  , C.Var 50 "counter"
  , C.Seq 50 51
  , C.Branch 51 (Eq (Num 0) (Num 1)) 53 56
  , C.Assign 53 "next_instr" (Num 0)
  , C.Seq 53 54
  , C.Assign 54 "stack_pointer" (Num 0)
  , C.Seq 54 55
  , C.Assign 55 "opcode" (Num 0)
  , C.Seq 55 56
  , C.Seq 55 35
  , C.Seq 55 57
  , C.Var 56 "NOP_56"
  , C.Seq 56 57
  , C.Var 57 "IF_ELSE_FOOTER"
  , C.Assign 58 "undefed" (Num 0)
  , C.Seq 58 59
  , C.Branch 59 (Eq (Num 0) (Num 1)) 60 61
  , C.Assign 60 "undefed" (Num 0)
  , C.Seq 60 59
  , C.Var 61 "LOOP_FOOTER"
  , C.Seq 61 62
  , C.Var 62 "lhs_o"
  , C.Seq 62 63
  , C.Var 63 "rhs_o"
  , C.Seq 63 64
  , C.Var 64 "res_o"
  , C.Seq 64 65
  , C.Assign 65 "stack_pointer" (Num 0)
  , C.Seq 65 66
  , C.Branch 66 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 68 68
  , C.Seq 67 1817
  , C.Seq 67 69
  , C.Var 68 "NOP_68"
  , C.Seq 68 69
  , C.Var 69 "IF_ELSE_FOOTER"
  , C.Assign 70 "res" (Num 0)
  , C.Seq 70 71
  , C.Var 71 "tmp"
  , C.Seq 71 72
  , C.Assign 72 "lhs" (Num 0)
  , C.Seq 72 73
  , C.Assign 73 "undefed" (Num 0)
  , C.Seq 73 74
  , C.Assign 74 "tmp" (Num 0)
  , C.Seq 74 75
  , C.Assign 75 "rhs" (Num 0)
  , C.Seq 75 76
  , C.Assign 76 "undefed" (Num 0)
  , C.Seq 76 77
  , C.Assign 77 "stack_pointer" (Num 0)
  , C.Seq 77 78
  , C.Assign 78 "stack_pointer" (Num 0)
  , C.Seq 78 79
  , C.Var 79 "word"
  , C.Seq 79 80
  , C.Assign 80 "opcode" (Num 0)
  , C.Seq 80 81
  , C.Assign 81 "oparg" (Num 0)
  , C.Seq 81 82
  , C.Branch 82 (Eq (Num 0) (Num 1)) 83 86
  , C.Var 83 "word"
  , C.Seq 83 84
  , C.Assign 84 "opcode" (Num 0)
  , C.Seq 84 85
  , C.Assign 85 "oparg" (Num 0)
  , C.Seq 85 86
  , C.Seq 85 82
  , C.Var 86 "LOOP_FOOTER"
  , C.Seq 86 87
  , C.Seq 86 35
  , C.Branch 87 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 89 129
  , C.Var 89 "NOP_89"
  , C.Var 90 "__CLABEL_TARGET_BINARY_OP_ADD_FLOAT"
  , C.Seq 90 91
  , C.Var 91 "this_instr"
  , C.Seq 91 92
  , C.Assign 92 "undefed" (Num 0)
  , C.Seq 92 93
  , C.Assign 93 "next_instr" (Num 0)
  , C.Seq 93 94
  , C.Var 94 "value"
  , C.Seq 94 95
  , C.Var 95 "left"
  , C.Seq 95 96
  , C.Var 96 "right"
  , C.Seq 96 97
  , C.Var 97 "res"
  , C.Seq 97 98
  , C.Assign 98 "value" (Num 0)
  , C.Seq 98 99
  , C.Var 99 "value_o"
  , C.Seq 99 100
  , C.Branch 100 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 102 102
  , C.Seq 101 42
  , C.Seq 101 103
  , C.Var 102 "NOP_102"
  , C.Seq 102 103
  , C.Var 103 "IF_ELSE_FOOTER"
  , C.Assign 104 "left" (Num 0)
  , C.Seq 104 105
  , C.Var 105 "left_o"
  , C.Seq 105 106
  , C.Branch 106 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 108 108
  , C.Seq 107 42
  , C.Seq 107 109
  , C.Var 108 "NOP_108"
  , C.Seq 108 109
  , C.Var 109 "IF_ELSE_FOOTER"
  , C.Assign 110 "right" (Num 0)
  , C.Seq 110 111
  , C.Var 111 "left_o"
  , C.Seq 111 112
  , C.Var 112 "right_o"
  , C.Seq 112 113
  , C.Var 113 "dres"
  , C.Seq 113 114
  , C.Assign 114 "res" (Num 0)
  , C.Seq 114 115
  , C.Branch 115 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 117 117
  , C.Seq 116 1813
  , C.Seq 116 118
  , C.Var 117 "NOP_117"
  , C.Seq 117 118
  , C.Var 118 "IF_ELSE_FOOTER"
  , C.Assign 119 "undefed" (Num 0)
  , C.Seq 119 120
  , C.Assign 120 "stack_pointer" (Num 0)
  , C.Seq 120 121
  , C.Var 121 "word"
  , C.Seq 121 122
  , C.Assign 122 "opcode" (Num 0)
  , C.Seq 122 123
  , C.Assign 123 "oparg" (Num 0)
  , C.Seq 123 124
  , C.Branch 124 (Eq (Num 0) (Num 1)) 125 128
  , C.Var 125 "word"
  , C.Seq 125 126
  , C.Assign 126 "opcode" (Num 0)
  , C.Seq 126 127
  , C.Assign 127 "oparg" (Num 0)
  , C.Seq 127 128
  , C.Seq 127 124
  , C.Var 128 "LOOP_FOOTER"
  , C.Seq 128 129
  , C.Seq 128 35
  , C.Branch 129 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 131 170
  , C.Var 131 "NOP_131"
  , C.Var 132 "__CLABEL_TARGET_BINARY_OP_ADD_INT"
  , C.Seq 132 133
  , C.Var 133 "this_instr"
  , C.Seq 133 134
  , C.Assign 134 "undefed" (Num 0)
  , C.Seq 134 135
  , C.Assign 135 "next_instr" (Num 0)
  , C.Seq 135 136
  , C.Var 136 "value"
  , C.Seq 136 137
  , C.Var 137 "left"
  , C.Seq 137 138
  , C.Var 138 "right"
  , C.Seq 138 139
  , C.Var 139 "res"
  , C.Seq 139 140
  , C.Assign 140 "value" (Num 0)
  , C.Seq 140 141
  , C.Var 141 "value_o"
  , C.Seq 141 142
  , C.Branch 142 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 144 144
  , C.Seq 143 42
  , C.Seq 143 145
  , C.Var 144 "NOP_144"
  , C.Seq 144 145
  , C.Var 145 "IF_ELSE_FOOTER"
  , C.Assign 146 "left" (Num 0)
  , C.Seq 146 147
  , C.Var 147 "left_o"
  , C.Seq 147 148
  , C.Branch 148 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 150 150
  , C.Seq 149 42
  , C.Seq 149 151
  , C.Var 150 "NOP_150"
  , C.Seq 150 151
  , C.Var 151 "IF_ELSE_FOOTER"
  , C.Assign 152 "right" (Num 0)
  , C.Seq 152 153
  , C.Var 153 "left_o"
  , C.Seq 153 154
  , C.Var 154 "right_o"
  , C.Seq 154 155
  , C.Assign 155 "res" (Num 0)
  , C.Seq 155 156
  , C.Branch 156 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 158 158
  , C.Seq 157 42
  , C.Seq 157 159
  , C.Var 158 "NOP_158"
  , C.Seq 158 159
  , C.Var 159 "IF_ELSE_FOOTER"
  , C.Assign 160 "undefed" (Num 0)
  , C.Seq 160 161
  , C.Assign 161 "stack_pointer" (Num 0)
  , C.Seq 161 162
  , C.Var 162 "word"
  , C.Seq 162 163
  , C.Assign 163 "opcode" (Num 0)
  , C.Seq 163 164
  , C.Assign 164 "oparg" (Num 0)
  , C.Seq 164 165
  , C.Branch 165 (Eq (Num 0) (Num 1)) 166 169
  , C.Var 166 "word"
  , C.Seq 166 167
  , C.Assign 167 "opcode" (Num 0)
  , C.Seq 167 168
  , C.Assign 168 "oparg" (Num 0)
  , C.Seq 168 169
  , C.Seq 168 165
  , C.Var 169 "LOOP_FOOTER"
  , C.Seq 169 170
  , C.Seq 169 35
  , C.Branch 170 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 172 214
  , C.Var 172 "NOP_172"
  , C.Var 173 "__CLABEL_TARGET_BINARY_OP_ADD_UNICODE"
  , C.Seq 173 174
  , C.Var 174 "this_instr"
  , C.Seq 174 175
  , C.Assign 175 "undefed" (Num 0)
  , C.Seq 175 176
  , C.Assign 176 "next_instr" (Num 0)
  , C.Seq 176 177
  , C.Var 177 "value"
  , C.Seq 177 178
  , C.Var 178 "nos"
  , C.Seq 178 179
  , C.Var 179 "left"
  , C.Seq 179 180
  , C.Var 180 "right"
  , C.Seq 180 181
  , C.Var 181 "res"
  , C.Seq 181 182
  , C.Assign 182 "value" (Num 0)
  , C.Seq 182 183
  , C.Var 183 "value_o"
  , C.Seq 183 184
  , C.Branch 184 (Eq (Plus (Plus (Num 0) (Plus (Id "PyUnicode_Type") (Num 0))) (Num 0)) (Num 1)) 186 186
  , C.Seq 185 42
  , C.Seq 185 187
  , C.Var 186 "NOP_186"
  , C.Seq 186 187
  , C.Var 187 "IF_ELSE_FOOTER"
  , C.Assign 188 "nos" (Num 0)
  , C.Seq 188 189
  , C.Var 189 "o"
  , C.Seq 189 190
  , C.Branch 190 (Eq (Plus (Plus (Num 0) (Plus (Id "PyUnicode_Type") (Num 0))) (Num 0)) (Num 1)) 192 192
  , C.Seq 191 42
  , C.Seq 191 193
  , C.Var 192 "NOP_192"
  , C.Seq 192 193
  , C.Var 193 "IF_ELSE_FOOTER"
  , C.Assign 194 "right" (Num 0)
  , C.Seq 194 195
  , C.Assign 195 "left" (Num 0)
  , C.Seq 195 196
  , C.Var 196 "left_o"
  , C.Seq 196 197
  , C.Var 197 "right_o"
  , C.Seq 197 198
  , C.Var 198 "res_o"
  , C.Seq 198 199
  , C.Branch 199 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 201 201
  , C.Seq 200 1813
  , C.Seq 200 202
  , C.Var 201 "NOP_201"
  , C.Seq 201 202
  , C.Var 202 "IF_ELSE_FOOTER"
  , C.Assign 203 "res" (Num 0)
  , C.Seq 203 204
  , C.Assign 204 "undefed" (Num 0)
  , C.Seq 204 205
  , C.Assign 205 "stack_pointer" (Num 0)
  , C.Seq 205 206
  , C.Var 206 "word"
  , C.Seq 206 207
  , C.Assign 207 "opcode" (Num 0)
  , C.Seq 207 208
  , C.Assign 208 "oparg" (Num 0)
  , C.Seq 208 209
  , C.Branch 209 (Eq (Num 0) (Num 1)) 210 213
  , C.Var 210 "word"
  , C.Seq 210 211
  , C.Assign 211 "opcode" (Num 0)
  , C.Seq 211 212
  , C.Assign 212 "oparg" (Num 0)
  , C.Seq 212 213
  , C.Seq 212 209
  , C.Var 213 "LOOP_FOOTER"
  , C.Seq 213 214
  , C.Seq 213 35
  , C.Branch 214 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 216 260
  , C.Var 216 "NOP_216"
  , C.Var 217 "__CLABEL_TARGET_BINARY_OP_EXTEND"
  , C.Seq 217 218
  , C.Var 218 "this_instr"
  , C.Seq 218 219
  , C.Assign 219 "undefed" (Num 0)
  , C.Seq 219 220
  , C.Assign 220 "next_instr" (Num 0)
  , C.Seq 220 221
  , C.Var 221 "left"
  , C.Seq 221 222
  , C.Var 222 "right"
  , C.Seq 222 223
  , C.Var 223 "res"
  , C.Seq 223 224
  , C.Assign 224 "right" (Num 0)
  , C.Seq 224 225
  , C.Assign 225 "left" (Num 0)
  , C.Seq 225 226
  , C.Var 226 "descr"
  , C.Seq 226 227
  , C.Var 227 "left_o"
  , C.Seq 227 228
  , C.Var 228 "right_o"
  , C.Seq 228 229
  , C.Var 229 "d"
  , C.Seq 229 230
  , C.Var 230 "res"
  , C.Seq 230 231
  , C.Assign 231 "stack_pointer" (Num 0)
  , C.Seq 231 232
  , C.Branch 232 (Eq (Plus (Id "res") (Num 0)) (Num 1)) 234 234
  , C.Seq 233 42
  , C.Seq 233 235
  , C.Var 234 "NOP_234"
  , C.Seq 234 235
  , C.Var 235 "IF_ELSE_FOOTER"
  , C.Var 236 "descr"
  , C.Seq 236 237
  , C.Var 237 "left_o"
  , C.Seq 237 238
  , C.Var 238 "right_o"
  , C.Seq 238 239
  , C.Var 239 "d"
  , C.Seq 239 240
  , C.Var 240 "res_o"
  , C.Seq 240 241
  , C.Var 241 "tmp"
  , C.Seq 241 242
  , C.Assign 242 "right" (Num 0)
  , C.Seq 242 243
  , C.Assign 243 "undefed" (Num 0)
  , C.Seq 243 244
  , C.Assign 244 "tmp" (Num 0)
  , C.Seq 244 245
  , C.Assign 245 "left" (Num 0)
  , C.Seq 245 246
  , C.Assign 246 "undefed" (Num 0)
  , C.Seq 246 247
  , C.Assign 247 "stack_pointer" (Num 0)
  , C.Seq 247 248
  , C.Assign 248 "stack_pointer" (Num 0)
  , C.Seq 248 249
  , C.Assign 249 "res" (Num 0)
  , C.Seq 249 250
  , C.Assign 250 "undefed" (Num 0)
  , C.Seq 250 251
  , C.Assign 251 "stack_pointer" (Num 0)
  , C.Seq 251 252
  , C.Var 252 "word"
  , C.Seq 252 253
  , C.Assign 253 "opcode" (Num 0)
  , C.Seq 253 254
  , C.Assign 254 "oparg" (Num 0)
  , C.Seq 254 255
  , C.Branch 255 (Eq (Num 0) (Num 1)) 256 259
  , C.Var 256 "word"
  , C.Seq 256 257
  , C.Assign 257 "opcode" (Num 0)
  , C.Seq 257 258
  , C.Assign 258 "oparg" (Num 0)
  , C.Seq 258 259
  , C.Seq 258 255
  , C.Var 259 "LOOP_FOOTER"
  , C.Seq 259 260
  , C.Seq 259 35
  , C.Branch 260 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 262 354
  , C.Var 262 "NOP_262"
  , C.Var 263 "__CLABEL_TARGET_BINARY_OP_INPLACE_ADD_UNICODE"
  , C.Seq 263 264
  , C.Var 264 "this_instr"
  , C.Seq 264 265
  , C.Assign 265 "undefed" (Num 0)
  , C.Seq 265 266
  , C.Assign 266 "next_instr" (Num 0)
  , C.Seq 266 267
  , C.Var 267 "value"
  , C.Seq 267 268
  , C.Var 268 "nos"
  , C.Seq 268 269
  , C.Var 269 "left"
  , C.Seq 269 270
  , C.Var 270 "right"
  , C.Seq 270 271
  , C.Assign 271 "value" (Num 0)
  , C.Seq 271 272
  , C.Var 272 "value_o"
  , C.Seq 272 273
  , C.Branch 273 (Eq (Plus (Plus (Num 0) (Plus (Id "PyUnicode_Type") (Num 0))) (Num 0)) (Num 1)) 275 275
  , C.Seq 274 42
  , C.Seq 274 276
  , C.Var 275 "NOP_275"
  , C.Seq 275 276
  , C.Var 276 "IF_ELSE_FOOTER"
  , C.Assign 277 "nos" (Num 0)
  , C.Seq 277 278
  , C.Var 278 "o"
  , C.Seq 278 279
  , C.Branch 279 (Eq (Plus (Plus (Num 0) (Plus (Id "PyUnicode_Type") (Num 0))) (Num 0)) (Num 1)) 281 281
  , C.Seq 280 42
  , C.Seq 280 282
  , C.Var 281 "NOP_281"
  , C.Seq 281 282
  , C.Var 282 "IF_ELSE_FOOTER"
  , C.Assign 283 "right" (Num 0)
  , C.Seq 283 284
  , C.Assign 284 "left" (Num 0)
  , C.Seq 284 285
  , C.Var 285 "left_o"
  , C.Seq 285 286
  , C.Var 286 "next_oparg"
  , C.Seq 286 287
  , C.Assign 287 "next_oparg" (Num 0)
  , C.Seq 287 288
  , C.Var 288 "target_local"
  , C.Seq 288 289
  , C.Branch 289 (Eq (Plus (Num 0) (Id "left_o")) (Num 1)) 291 291
  , C.Seq 290 42
  , C.Seq 290 292
  , C.Var 291 "NOP_291"
  , C.Seq 291 292
  , C.Var 292 "IF_ELSE_FOOTER"
  , C.Var 293 "temp"
  , C.Seq 293 294
  , C.Var 294 "right_o"
  , C.Seq 294 295
  , C.Assign 295 "stack_pointer" (Num 0)
  , C.Seq 295 296
  , C.Assign 296 "stack_pointer" (Num 0)
  , C.Seq 296 297
  , C.Assign 297 "undefed" (Num 0)
  , C.Seq 297 298
  , C.Var 298 "op"
  , C.Seq 298 299
  , C.Branch 299 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 301 316
  , C.Var 301 "tracer"
  , C.Seq 301 302
  , C.Branch 302 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 304 305
  , C.Var 304 "data"
  , C.Seq 304 305
  , C.Seq 304 306
  , C.Var 305 "NOP_305"
  , C.Seq 305 306
  , C.Var 306 "IF_ELSE_FOOTER"
  , C.Branch 307 (Eq (Num 0) (Num 1)) 308 314
  , C.Var 308 "tracer"
  , C.Seq 308 309
  , C.Branch 309 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 311 312
  , C.Var 311 "data"
  , C.Seq 311 312
  , C.Seq 311 313
  , C.Var 312 "NOP_312"
  , C.Seq 312 313
  , C.Var 313 "IF_ELSE_FOOTER"
  , C.Seq 313 307
  , C.Var 314 "LOOP_FOOTER"
  , C.Seq 314 315
  , C.Var 315 "dealloc"
  , C.Seq 315 316
  , C.Seq 315 317
  , C.Var 316 "NOP_316"
  , C.Seq 316 317
  , C.Var 317 "IF_ELSE_FOOTER"
  , C.Branch 318 (Eq (Num 0) (Num 1)) 319 339
  , C.Var 319 "op"
  , C.Seq 319 320
  , C.Branch 320 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 322 337
  , C.Var 322 "tracer"
  , C.Seq 322 323
  , C.Branch 323 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 325 326
  , C.Var 325 "data"
  , C.Seq 325 326
  , C.Seq 325 327
  , C.Var 326 "NOP_326"
  , C.Seq 326 327
  , C.Var 327 "IF_ELSE_FOOTER"
  , C.Branch 328 (Eq (Num 0) (Num 1)) 329 335
  , C.Var 329 "tracer"
  , C.Seq 329 330
  , C.Branch 330 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 332 333
  , C.Var 332 "data"
  , C.Seq 332 333
  , C.Seq 332 334
  , C.Var 333 "NOP_333"
  , C.Seq 333 334
  , C.Var 334 "IF_ELSE_FOOTER"
  , C.Seq 334 328
  , C.Var 335 "LOOP_FOOTER"
  , C.Seq 335 336
  , C.Var 336 "dealloc"
  , C.Seq 336 337
  , C.Seq 336 338
  , C.Var 337 "NOP_337"
  , C.Seq 337 338
  , C.Var 338 "IF_ELSE_FOOTER"
  , C.Seq 338 318
  , C.Var 339 "LOOP_FOOTER"
  , C.Seq 339 340
  , C.Assign 340 "stack_pointer" (Num 0)
  , C.Seq 340 341
  , C.Branch 341 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 343 343
  , C.Seq 342 1817
  , C.Seq 342 344
  , C.Var 343 "NOP_343"
  , C.Seq 343 344
  , C.Var 344 "IF_ELSE_FOOTER"
  , C.Assign 345 "next_instr" (Num 0)
  , C.Seq 345 346
  , C.Var 346 "word"
  , C.Seq 346 347
  , C.Assign 347 "opcode" (Num 0)
  , C.Seq 347 348
  , C.Assign 348 "oparg" (Num 0)
  , C.Seq 348 349
  , C.Branch 349 (Eq (Num 0) (Num 1)) 350 353
  , C.Var 350 "word"
  , C.Seq 350 351
  , C.Assign 351 "opcode" (Num 0)
  , C.Seq 351 352
  , C.Assign 352 "oparg" (Num 0)
  , C.Seq 352 353
  , C.Seq 352 349
  , C.Var 353 "LOOP_FOOTER"
  , C.Seq 353 354
  , C.Seq 353 35
  , C.Branch 354 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 356 396
  , C.Var 356 "NOP_356"
  , C.Var 357 "__CLABEL_TARGET_BINARY_OP_MULTIPLY_FLOAT"
  , C.Seq 357 358
  , C.Var 358 "this_instr"
  , C.Seq 358 359
  , C.Assign 359 "undefed" (Num 0)
  , C.Seq 359 360
  , C.Assign 360 "next_instr" (Num 0)
  , C.Seq 360 361
  , C.Var 361 "value"
  , C.Seq 361 362
  , C.Var 362 "left"
  , C.Seq 362 363
  , C.Var 363 "right"
  , C.Seq 363 364
  , C.Var 364 "res"
  , C.Seq 364 365
  , C.Assign 365 "value" (Num 0)
  , C.Seq 365 366
  , C.Var 366 "value_o"
  , C.Seq 366 367
  , C.Branch 367 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 369 369
  , C.Seq 368 42
  , C.Seq 368 370
  , C.Var 369 "NOP_369"
  , C.Seq 369 370
  , C.Var 370 "IF_ELSE_FOOTER"
  , C.Assign 371 "left" (Num 0)
  , C.Seq 371 372
  , C.Var 372 "left_o"
  , C.Seq 372 373
  , C.Branch 373 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 375 375
  , C.Seq 374 42
  , C.Seq 374 376
  , C.Var 375 "NOP_375"
  , C.Seq 375 376
  , C.Var 376 "IF_ELSE_FOOTER"
  , C.Assign 377 "right" (Num 0)
  , C.Seq 377 378
  , C.Var 378 "left_o"
  , C.Seq 378 379
  , C.Var 379 "right_o"
  , C.Seq 379 380
  , C.Var 380 "dres"
  , C.Seq 380 381
  , C.Assign 381 "res" (Num 0)
  , C.Seq 381 382
  , C.Branch 382 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 384 384
  , C.Seq 383 1813
  , C.Seq 383 385
  , C.Var 384 "NOP_384"
  , C.Seq 384 385
  , C.Var 385 "IF_ELSE_FOOTER"
  , C.Assign 386 "undefed" (Num 0)
  , C.Seq 386 387
  , C.Assign 387 "stack_pointer" (Num 0)
  , C.Seq 387 388
  , C.Var 388 "word"
  , C.Seq 388 389
  , C.Assign 389 "opcode" (Num 0)
  , C.Seq 389 390
  , C.Assign 390 "oparg" (Num 0)
  , C.Seq 390 391
  , C.Branch 391 (Eq (Num 0) (Num 1)) 392 395
  , C.Var 392 "word"
  , C.Seq 392 393
  , C.Assign 393 "opcode" (Num 0)
  , C.Seq 393 394
  , C.Assign 394 "oparg" (Num 0)
  , C.Seq 394 395
  , C.Seq 394 391
  , C.Var 395 "LOOP_FOOTER"
  , C.Seq 395 396
  , C.Seq 395 35
  , C.Branch 396 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 398 437
  , C.Var 398 "NOP_398"
  , C.Var 399 "__CLABEL_TARGET_BINARY_OP_MULTIPLY_INT"
  , C.Seq 399 400
  , C.Var 400 "this_instr"
  , C.Seq 400 401
  , C.Assign 401 "undefed" (Num 0)
  , C.Seq 401 402
  , C.Assign 402 "next_instr" (Num 0)
  , C.Seq 402 403
  , C.Var 403 "value"
  , C.Seq 403 404
  , C.Var 404 "left"
  , C.Seq 404 405
  , C.Var 405 "right"
  , C.Seq 405 406
  , C.Var 406 "res"
  , C.Seq 406 407
  , C.Assign 407 "value" (Num 0)
  , C.Seq 407 408
  , C.Var 408 "value_o"
  , C.Seq 408 409
  , C.Branch 409 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 411 411
  , C.Seq 410 42
  , C.Seq 410 412
  , C.Var 411 "NOP_411"
  , C.Seq 411 412
  , C.Var 412 "IF_ELSE_FOOTER"
  , C.Assign 413 "left" (Num 0)
  , C.Seq 413 414
  , C.Var 414 "left_o"
  , C.Seq 414 415
  , C.Branch 415 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 417 417
  , C.Seq 416 42
  , C.Seq 416 418
  , C.Var 417 "NOP_417"
  , C.Seq 417 418
  , C.Var 418 "IF_ELSE_FOOTER"
  , C.Assign 419 "right" (Num 0)
  , C.Seq 419 420
  , C.Var 420 "left_o"
  , C.Seq 420 421
  , C.Var 421 "right_o"
  , C.Seq 421 422
  , C.Assign 422 "res" (Num 0)
  , C.Seq 422 423
  , C.Branch 423 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 425 425
  , C.Seq 424 42
  , C.Seq 424 426
  , C.Var 425 "NOP_425"
  , C.Seq 425 426
  , C.Var 426 "IF_ELSE_FOOTER"
  , C.Assign 427 "undefed" (Num 0)
  , C.Seq 427 428
  , C.Assign 428 "stack_pointer" (Num 0)
  , C.Seq 428 429
  , C.Var 429 "word"
  , C.Seq 429 430
  , C.Assign 430 "opcode" (Num 0)
  , C.Seq 430 431
  , C.Assign 431 "oparg" (Num 0)
  , C.Seq 431 432
  , C.Branch 432 (Eq (Num 0) (Num 1)) 433 436
  , C.Var 433 "word"
  , C.Seq 433 434
  , C.Assign 434 "opcode" (Num 0)
  , C.Seq 434 435
  , C.Assign 435 "oparg" (Num 0)
  , C.Seq 435 436
  , C.Seq 435 432
  , C.Var 436 "LOOP_FOOTER"
  , C.Seq 436 437
  , C.Seq 436 35
  , C.Branch 437 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 439 489
  , C.Var 439 "NOP_439"
  , C.Var 440 "__CLABEL_TARGET_BINARY_OP_SUBSCR_DICT"
  , C.Seq 440 441
  , C.Var 441 "this_instr"
  , C.Seq 441 442
  , C.Assign 442 "undefed" (Num 0)
  , C.Seq 442 443
  , C.Assign 443 "next_instr" (Num 0)
  , C.Seq 443 444
  , C.Var 444 "nos"
  , C.Seq 444 445
  , C.Var 445 "dict_st"
  , C.Seq 445 446
  , C.Var 446 "sub_st"
  , C.Seq 446 447
  , C.Var 447 "res"
  , C.Seq 447 448
  , C.Assign 448 "nos" (Num 0)
  , C.Seq 448 449
  , C.Var 449 "o"
  , C.Seq 449 450
  , C.Branch 450 (Eq (Plus (Plus (Num 0) (Plus (Id "PyDict_Type") (Num 0))) (Num 0)) (Num 1)) 452 452
  , C.Seq 451 42
  , C.Seq 451 453
  , C.Var 452 "NOP_452"
  , C.Seq 452 453
  , C.Var 453 "IF_ELSE_FOOTER"
  , C.Assign 454 "sub_st" (Num 0)
  , C.Seq 454 455
  , C.Assign 455 "dict_st" (Num 0)
  , C.Seq 455 456
  , C.Var 456 "sub"
  , C.Seq 456 457
  , C.Var 457 "dict"
  , C.Seq 457 458
  , C.Var 458 "res_o"
  , C.Seq 458 459
  , C.Var 459 "rc"
  , C.Seq 459 460
  , C.Assign 460 "stack_pointer" (Num 0)
  , C.Seq 460 461
  , C.Branch 461 (Eq (Plus (Id "rc") (Num 0)) (Num 1)) 463 464
  , C.Assign 463 "stack_pointer" (Num 0)
  , C.Seq 463 465
  , C.Var 464 "NOP_464"
  , C.Seq 464 465
  , C.Var 465 "IF_ELSE_FOOTER"
  , C.Var 466 "tmp"
  , C.Seq 466 467
  , C.Assign 467 "sub_st" (Num 0)
  , C.Seq 467 468
  , C.Assign 468 "undefed" (Num 0)
  , C.Seq 468 469
  , C.Assign 469 "tmp" (Num 0)
  , C.Seq 469 470
  , C.Assign 470 "dict_st" (Num 0)
  , C.Seq 470 471
  , C.Assign 471 "undefed" (Num 0)
  , C.Seq 471 472
  , C.Assign 472 "stack_pointer" (Num 0)
  , C.Seq 472 473
  , C.Assign 473 "stack_pointer" (Num 0)
  , C.Seq 473 474
  , C.Branch 474 (Eq (Plus (Id "rc") (Num 0)) (Num 1)) 476 476
  , C.Seq 475 1817
  , C.Seq 475 477
  , C.Var 476 "NOP_476"
  , C.Seq 476 477
  , C.Var 477 "IF_ELSE_FOOTER"
  , C.Assign 478 "res" (Num 0)
  , C.Seq 478 479
  , C.Assign 479 "undefed" (Num 0)
  , C.Seq 479 480
  , C.Assign 480 "stack_pointer" (Num 0)
  , C.Seq 480 481
  , C.Var 481 "word"
  , C.Seq 481 482
  , C.Assign 482 "opcode" (Num 0)
  , C.Seq 482 483
  , C.Assign 483 "oparg" (Num 0)
  , C.Seq 483 484
  , C.Branch 484 (Eq (Num 0) (Num 1)) 485 488
  , C.Var 485 "word"
  , C.Seq 485 486
  , C.Assign 486 "opcode" (Num 0)
  , C.Seq 486 487
  , C.Assign 487 "oparg" (Num 0)
  , C.Seq 487 488
  , C.Seq 487 484
  , C.Var 488 "LOOP_FOOTER"
  , C.Seq 488 489
  , C.Seq 488 35
  , C.Branch 489 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 491 549
  , C.Var 491 "NOP_491"
  , C.Var 492 "__CLABEL_TARGET_BINARY_OP_SUBSCR_GETITEM"
  , C.Seq 492 493
  , C.Var 493 "this_instr"
  , C.Seq 493 494
  , C.Assign 494 "undefed" (Num 0)
  , C.Seq 494 495
  , C.Assign 495 "next_instr" (Num 0)
  , C.Seq 495 496
  , C.Var 496 "container"
  , C.Seq 496 497
  , C.Var 497 "getitem"
  , C.Seq 497 498
  , C.Var 498 "sub"
  , C.Seq 498 499
  , C.Var 499 "new_frame"
  , C.Seq 499 500
  , C.Branch 500 (Eq (Num 0) (Num 1)) 502 502
  , C.Seq 501 42
  , C.Seq 501 503
  , C.Var 502 "NOP_502"
  , C.Seq 502 503
  , C.Var 503 "IF_ELSE_FOOTER"
  , C.Assign 504 "container" (Num 0)
  , C.Seq 504 505
  , C.Var 505 "tp"
  , C.Seq 505 506
  , C.Branch 506 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 508 508
  , C.Seq 507 42
  , C.Seq 507 509
  , C.Var 508 "NOP_508"
  , C.Seq 508 509
  , C.Var 509 "IF_ELSE_FOOTER"
  , C.Var 510 "ht"
  , C.Seq 510 511
  , C.Var 511 "getitem_o"
  , C.Seq 511 512
  , C.Branch 512 (Eq (Plus (Id "getitem_o") (Num 0)) (Num 1)) 514 514
  , C.Seq 513 42
  , C.Seq 513 515
  , C.Var 514 "NOP_514"
  , C.Seq 514 515
  , C.Var 515 "IF_ELSE_FOOTER"
  , C.Var 516 "cached_version"
  , C.Seq 516 517
  , C.Branch 517 (Eq (Plus (Num 0) (Id "cached_version")) (Num 1)) 519 519
  , C.Seq 518 42
  , C.Seq 518 520
  , C.Var 519 "NOP_519"
  , C.Seq 519 520
  , C.Var 520 "IF_ELSE_FOOTER"
  , C.Var 521 "code"
  , C.Seq 521 522
  , C.Branch 522 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 524 524
  , C.Seq 523 42
  , C.Seq 523 525
  , C.Var 524 "NOP_524"
  , C.Seq 524 525
  , C.Var 525 "IF_ELSE_FOOTER"
  , C.Assign 526 "getitem" (Num 0)
  , C.Seq 526 527
  , C.Assign 527 "sub" (Num 0)
  , C.Seq 527 528
  , C.Var 528 "pushed_frame"
  , C.Seq 528 529
  , C.Assign 529 "undefed" (Num 0)
  , C.Seq 529 530
  , C.Assign 530 "undefed" (Num 0)
  , C.Seq 530 531
  , C.Assign 531 "undefed" (Num 0)
  , C.Seq 531 532
  , C.Assign 532 "new_frame" (Num 0)
  , C.Seq 532 533
  , C.Var 533 "temp"
  , C.Seq 533 534
  , C.Assign 534 "stack_pointer" (Num 0)
  , C.Seq 534 535
  , C.Assign 535 "frame" (Num 0)
  , C.Seq 535 536
  , C.Assign 536 "stack_pointer" (Num 0)
  , C.Seq 536 537
  , C.Assign 537 "next_instr" (Num 0)
  , C.Seq 537 538
  , C.Branch 538 (Eq (Num 0) (Num 1)) 539 540
  , C.Assign 539 "next_instr" (Num 0)
  , C.Seq 539 538
  , C.Var 540 "LOOP_FOOTER"
  , C.Seq 540 541
  , C.Var 541 "word"
  , C.Seq 541 542
  , C.Assign 542 "opcode" (Num 0)
  , C.Seq 542 543
  , C.Assign 543 "oparg" (Num 0)
  , C.Seq 543 544
  , C.Branch 544 (Eq (Num 0) (Num 1)) 545 548
  , C.Var 545 "word"
  , C.Seq 545 546
  , C.Assign 546 "opcode" (Num 0)
  , C.Seq 546 547
  , C.Assign 547 "oparg" (Num 0)
  , C.Seq 547 548
  , C.Seq 547 544
  , C.Var 548 "LOOP_FOOTER"
  , C.Seq 548 549
  , C.Seq 548 35
  , C.Branch 549 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 551 604
  , C.Var 551 "NOP_551"
  , C.Var 552 "__CLABEL_TARGET_BINARY_OP_SUBSCR_LIST_INT"
  , C.Seq 552 553
  , C.Var 553 "this_instr"
  , C.Seq 553 554
  , C.Assign 554 "undefed" (Num 0)
  , C.Seq 554 555
  , C.Assign 555 "next_instr" (Num 0)
  , C.Seq 555 556
  , C.Var 556 "value"
  , C.Seq 556 557
  , C.Var 557 "nos"
  , C.Seq 557 558
  , C.Var 558 "list_st"
  , C.Seq 558 559
  , C.Var 559 "sub_st"
  , C.Seq 559 560
  , C.Var 560 "res"
  , C.Seq 560 561
  , C.Assign 561 "value" (Num 0)
  , C.Seq 561 562
  , C.Var 562 "value_o"
  , C.Seq 562 563
  , C.Branch 563 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 565 565
  , C.Seq 564 42
  , C.Seq 564 566
  , C.Var 565 "NOP_565"
  , C.Seq 565 566
  , C.Var 566 "IF_ELSE_FOOTER"
  , C.Assign 567 "nos" (Num 0)
  , C.Seq 567 568
  , C.Var 568 "o"
  , C.Seq 568 569
  , C.Branch 569 (Eq (Plus (Plus (Num 0) (Plus (Id "PyList_Type") (Num 0))) (Num 0)) (Num 1)) 571 571
  , C.Seq 570 42
  , C.Seq 570 572
  , C.Var 571 "NOP_571"
  , C.Seq 571 572
  , C.Var 572 "IF_ELSE_FOOTER"
  , C.Assign 573 "sub_st" (Num 0)
  , C.Seq 573 574
  , C.Assign 574 "list_st" (Num 0)
  , C.Seq 574 575
  , C.Var 575 "sub"
  , C.Seq 575 576
  , C.Var 576 "list"
  , C.Seq 576 577
  , C.Branch 577 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 579 579
  , C.Seq 578 42
  , C.Seq 578 580
  , C.Var 579 "NOP_579"
  , C.Seq 579 580
  , C.Var 580 "IF_ELSE_FOOTER"
  , C.Var 581 "index"
  , C.Seq 581 582
  , C.Branch 582 (Eq (Plus (Id "index") (Num 0)) (Num 1)) 584 584
  , C.Seq 583 42
  , C.Seq 583 585
  , C.Var 584 "NOP_584"
  , C.Seq 584 585
  , C.Var 585 "IF_ELSE_FOOTER"
  , C.Var 586 "res_o"
  , C.Seq 586 587
  , C.Assign 587 "res" (Num 0)
  , C.Seq 587 588
  , C.Var 588 "tmp"
  , C.Seq 588 589
  , C.Assign 589 "list_st" (Num 0)
  , C.Seq 589 590
  , C.Assign 590 "undefed" (Num 0)
  , C.Seq 590 591
  , C.Assign 591 "tmp" (Num 0)
  , C.Seq 591 592
  , C.Assign 592 "sub_st" (Num 0)
  , C.Seq 592 593
  , C.Assign 593 "undefed" (Num 0)
  , C.Seq 593 594
  , C.Assign 594 "stack_pointer" (Num 0)
  , C.Seq 594 595
  , C.Assign 595 "stack_pointer" (Num 0)
  , C.Seq 595 596
  , C.Var 596 "word"
  , C.Seq 596 597
  , C.Assign 597 "opcode" (Num 0)
  , C.Seq 597 598
  , C.Assign 598 "oparg" (Num 0)
  , C.Seq 598 599
  , C.Branch 599 (Eq (Num 0) (Num 1)) 600 603
  , C.Var 600 "word"
  , C.Seq 600 601
  , C.Assign 601 "opcode" (Num 0)
  , C.Seq 601 602
  , C.Assign 602 "oparg" (Num 0)
  , C.Seq 602 603
  , C.Seq 602 599
  , C.Var 603 "LOOP_FOOTER"
  , C.Seq 603 604
  , C.Seq 603 35
  , C.Branch 604 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 606 657
  , C.Var 606 "NOP_606"
  , C.Var 607 "__CLABEL_TARGET_BINARY_OP_SUBSCR_LIST_SLICE"
  , C.Seq 607 608
  , C.Var 608 "this_instr"
  , C.Seq 608 609
  , C.Assign 609 "undefed" (Num 0)
  , C.Seq 609 610
  , C.Assign 610 "next_instr" (Num 0)
  , C.Seq 610 611
  , C.Var 611 "tos"
  , C.Seq 611 612
  , C.Var 612 "nos"
  , C.Seq 612 613
  , C.Var 613 "list_st"
  , C.Seq 613 614
  , C.Var 614 "sub_st"
  , C.Seq 614 615
  , C.Var 615 "res"
  , C.Seq 615 616
  , C.Assign 616 "tos" (Num 0)
  , C.Seq 616 617
  , C.Var 617 "o"
  , C.Seq 617 618
  , C.Branch 618 (Eq (Plus (Plus (Num 0) (Plus (Id "PySlice_Type") (Num 0))) (Num 0)) (Num 1)) 620 620
  , C.Seq 619 42
  , C.Seq 619 621
  , C.Var 620 "NOP_620"
  , C.Seq 620 621
  , C.Var 621 "IF_ELSE_FOOTER"
  , C.Assign 622 "nos" (Num 0)
  , C.Seq 622 623
  , C.Var 623 "o"
  , C.Seq 623 624
  , C.Branch 624 (Eq (Plus (Plus (Num 0) (Plus (Id "PyList_Type") (Num 0))) (Num 0)) (Num 1)) 626 626
  , C.Seq 625 42
  , C.Seq 625 627
  , C.Var 626 "NOP_626"
  , C.Seq 626 627
  , C.Var 627 "IF_ELSE_FOOTER"
  , C.Assign 628 "sub_st" (Num 0)
  , C.Seq 628 629
  , C.Assign 629 "list_st" (Num 0)
  , C.Seq 629 630
  , C.Var 630 "sub"
  , C.Seq 630 631
  , C.Var 631 "list"
  , C.Seq 631 632
  , C.Var 632 "res_o"
  , C.Seq 632 633
  , C.Assign 633 "stack_pointer" (Num 0)
  , C.Seq 633 634
  , C.Var 634 "tmp"
  , C.Seq 634 635
  , C.Assign 635 "sub_st" (Num 0)
  , C.Seq 635 636
  , C.Assign 636 "undefed" (Num 0)
  , C.Seq 636 637
  , C.Assign 637 "tmp" (Num 0)
  , C.Seq 637 638
  , C.Assign 638 "list_st" (Num 0)
  , C.Seq 638 639
  , C.Assign 639 "undefed" (Num 0)
  , C.Seq 639 640
  , C.Assign 640 "stack_pointer" (Num 0)
  , C.Seq 640 641
  , C.Assign 641 "stack_pointer" (Num 0)
  , C.Seq 641 642
  , C.Branch 642 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 644 644
  , C.Seq 643 1817
  , C.Seq 643 645
  , C.Var 644 "NOP_644"
  , C.Seq 644 645
  , C.Var 645 "IF_ELSE_FOOTER"
  , C.Assign 646 "res" (Num 0)
  , C.Seq 646 647
  , C.Assign 647 "undefed" (Num 0)
  , C.Seq 647 648
  , C.Assign 648 "stack_pointer" (Num 0)
  , C.Seq 648 649
  , C.Var 649 "word"
  , C.Seq 649 650
  , C.Assign 650 "opcode" (Num 0)
  , C.Seq 650 651
  , C.Assign 651 "oparg" (Num 0)
  , C.Seq 651 652
  , C.Branch 652 (Eq (Num 0) (Num 1)) 653 656
  , C.Var 653 "word"
  , C.Seq 653 654
  , C.Assign 654 "opcode" (Num 0)
  , C.Seq 654 655
  , C.Assign 655 "oparg" (Num 0)
  , C.Seq 655 656
  , C.Seq 655 652
  , C.Var 656 "LOOP_FOOTER"
  , C.Seq 656 657
  , C.Seq 656 35
  , C.Branch 657 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 659 713
  , C.Var 659 "NOP_659"
  , C.Var 660 "__CLABEL_TARGET_BINARY_OP_SUBSCR_STR_INT"
  , C.Seq 660 661
  , C.Var 661 "this_instr"
  , C.Seq 661 662
  , C.Assign 662 "undefed" (Num 0)
  , C.Seq 662 663
  , C.Assign 663 "next_instr" (Num 0)
  , C.Seq 663 664
  , C.Var 664 "value"
  , C.Seq 664 665
  , C.Var 665 "nos"
  , C.Seq 665 666
  , C.Var 666 "str_st"
  , C.Seq 666 667
  , C.Var 667 "sub_st"
  , C.Seq 667 668
  , C.Var 668 "res"
  , C.Seq 668 669
  , C.Assign 669 "value" (Num 0)
  , C.Seq 669 670
  , C.Var 670 "value_o"
  , C.Seq 670 671
  , C.Branch 671 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 673 673
  , C.Seq 672 42
  , C.Seq 672 674
  , C.Var 673 "NOP_673"
  , C.Seq 673 674
  , C.Var 674 "IF_ELSE_FOOTER"
  , C.Assign 675 "nos" (Num 0)
  , C.Seq 675 676
  , C.Var 676 "o"
  , C.Seq 676 677
  , C.Branch 677 (Eq (Plus (Plus (Num 0) (Plus (Id "PyUnicode_Type") (Num 0))) (Num 0)) (Num 1)) 679 679
  , C.Seq 678 42
  , C.Seq 678 680
  , C.Var 679 "NOP_679"
  , C.Seq 679 680
  , C.Var 680 "IF_ELSE_FOOTER"
  , C.Assign 681 "sub_st" (Num 0)
  , C.Seq 681 682
  , C.Assign 682 "str_st" (Num 0)
  , C.Seq 682 683
  , C.Var 683 "sub"
  , C.Seq 683 684
  , C.Var 684 "str"
  , C.Seq 684 685
  , C.Branch 685 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 687 687
  , C.Seq 686 42
  , C.Seq 686 688
  , C.Var 687 "NOP_687"
  , C.Seq 687 688
  , C.Var 688 "IF_ELSE_FOOTER"
  , C.Var 689 "index"
  , C.Seq 689 690
  , C.Branch 690 (Eq (Plus (Num 0) (Id "index")) (Num 1)) 692 692
  , C.Seq 691 42
  , C.Seq 691 693
  , C.Var 692 "NOP_692"
  , C.Seq 692 693
  , C.Var 693 "IF_ELSE_FOOTER"
  , C.Var 694 "c"
  , C.Seq 694 695
  , C.Branch 695 (Eq (Plus (Plus (Plus (Num 0) (Num 0)) (Plus (Num 0) (Num 0))) (Id "c")) (Num 1)) 697 697
  , C.Seq 696 42
  , C.Seq 696 698
  , C.Var 697 "NOP_697"
  , C.Seq 697 698
  , C.Var 698 "IF_ELSE_FOOTER"
  , C.Var 699 "res_o"
  , C.Seq 699 700
  , C.Assign 700 "stack_pointer" (Num 0)
  , C.Seq 700 701
  , C.Assign 701 "stack_pointer" (Num 0)
  , C.Seq 701 702
  , C.Assign 702 "res" (Num 0)
  , C.Seq 702 703
  , C.Assign 703 "undefed" (Num 0)
  , C.Seq 703 704
  , C.Assign 704 "stack_pointer" (Num 0)
  , C.Seq 704 705
  , C.Var 705 "word"
  , C.Seq 705 706
  , C.Assign 706 "opcode" (Num 0)
  , C.Seq 706 707
  , C.Assign 707 "oparg" (Num 0)
  , C.Seq 707 708
  , C.Branch 708 (Eq (Num 0) (Num 1)) 709 712
  , C.Var 709 "word"
  , C.Seq 709 710
  , C.Assign 710 "opcode" (Num 0)
  , C.Seq 710 711
  , C.Assign 711 "oparg" (Num 0)
  , C.Seq 711 712
  , C.Seq 711 708
  , C.Var 712 "LOOP_FOOTER"
  , C.Seq 712 713
  , C.Seq 712 35
  , C.Branch 713 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 715 765
  , C.Var 715 "NOP_715"
  , C.Var 716 "__CLABEL_TARGET_BINARY_OP_SUBSCR_TUPLE_INT"
  , C.Seq 716 717
  , C.Var 717 "this_instr"
  , C.Seq 717 718
  , C.Assign 718 "undefed" (Num 0)
  , C.Seq 718 719
  , C.Assign 719 "next_instr" (Num 0)
  , C.Seq 719 720
  , C.Var 720 "value"
  , C.Seq 720 721
  , C.Var 721 "nos"
  , C.Seq 721 722
  , C.Var 722 "tuple_st"
  , C.Seq 722 723
  , C.Var 723 "sub_st"
  , C.Seq 723 724
  , C.Var 724 "res"
  , C.Seq 724 725
  , C.Assign 725 "value" (Num 0)
  , C.Seq 725 726
  , C.Var 726 "value_o"
  , C.Seq 726 727
  , C.Branch 727 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 729 729
  , C.Seq 728 42
  , C.Seq 728 730
  , C.Var 729 "NOP_729"
  , C.Seq 729 730
  , C.Var 730 "IF_ELSE_FOOTER"
  , C.Assign 731 "nos" (Num 0)
  , C.Seq 731 732
  , C.Var 732 "o"
  , C.Seq 732 733
  , C.Branch 733 (Eq (Plus (Plus (Num 0) (Plus (Id "PyTuple_Type") (Num 0))) (Num 0)) (Num 1)) 735 735
  , C.Seq 734 42
  , C.Seq 734 736
  , C.Var 735 "NOP_735"
  , C.Seq 735 736
  , C.Var 736 "IF_ELSE_FOOTER"
  , C.Assign 737 "sub_st" (Num 0)
  , C.Seq 737 738
  , C.Assign 738 "tuple_st" (Num 0)
  , C.Seq 738 739
  , C.Var 739 "sub"
  , C.Seq 739 740
  , C.Var 740 "tuple"
  , C.Seq 740 741
  , C.Branch 741 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 743 743
  , C.Seq 742 42
  , C.Seq 742 744
  , C.Var 743 "NOP_743"
  , C.Seq 743 744
  , C.Var 744 "IF_ELSE_FOOTER"
  , C.Var 745 "index"
  , C.Seq 745 746
  , C.Branch 746 (Eq (Plus (Id "index") (Num 0)) (Num 1)) 748 748
  , C.Seq 747 42
  , C.Seq 747 749
  , C.Var 748 "NOP_748"
  , C.Seq 748 749
  , C.Var 749 "IF_ELSE_FOOTER"
  , C.Var 750 "res_o"
  , C.Seq 750 751
  , C.Assign 751 "res" (Num 0)
  , C.Seq 751 752
  , C.Assign 752 "stack_pointer" (Num 0)
  , C.Seq 752 753
  , C.Var 753 "tmp"
  , C.Seq 753 754
  , C.Assign 754 "tuple_st" (Num 0)
  , C.Seq 754 755
  , C.Assign 755 "undefed" (Num 0)
  , C.Seq 755 756
  , C.Assign 756 "stack_pointer" (Num 0)
  , C.Seq 756 757
  , C.Var 757 "word"
  , C.Seq 757 758
  , C.Assign 758 "opcode" (Num 0)
  , C.Seq 758 759
  , C.Assign 759 "oparg" (Num 0)
  , C.Seq 759 760
  , C.Branch 760 (Eq (Num 0) (Num 1)) 761 764
  , C.Var 761 "word"
  , C.Seq 761 762
  , C.Assign 762 "opcode" (Num 0)
  , C.Seq 762 763
  , C.Assign 763 "oparg" (Num 0)
  , C.Seq 763 764
  , C.Seq 763 760
  , C.Var 764 "LOOP_FOOTER"
  , C.Seq 764 765
  , C.Seq 764 35
  , C.Branch 765 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 767 807
  , C.Var 767 "NOP_767"
  , C.Var 768 "__CLABEL_TARGET_BINARY_OP_SUBTRACT_FLOAT"
  , C.Seq 768 769
  , C.Var 769 "this_instr"
  , C.Seq 769 770
  , C.Assign 770 "undefed" (Num 0)
  , C.Seq 770 771
  , C.Assign 771 "next_instr" (Num 0)
  , C.Seq 771 772
  , C.Var 772 "value"
  , C.Seq 772 773
  , C.Var 773 "left"
  , C.Seq 773 774
  , C.Var 774 "right"
  , C.Seq 774 775
  , C.Var 775 "res"
  , C.Seq 775 776
  , C.Assign 776 "value" (Num 0)
  , C.Seq 776 777
  , C.Var 777 "value_o"
  , C.Seq 777 778
  , C.Branch 778 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 780 780
  , C.Seq 779 42
  , C.Seq 779 781
  , C.Var 780 "NOP_780"
  , C.Seq 780 781
  , C.Var 781 "IF_ELSE_FOOTER"
  , C.Assign 782 "left" (Num 0)
  , C.Seq 782 783
  , C.Var 783 "left_o"
  , C.Seq 783 784
  , C.Branch 784 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 786 786
  , C.Seq 785 42
  , C.Seq 785 787
  , C.Var 786 "NOP_786"
  , C.Seq 786 787
  , C.Var 787 "IF_ELSE_FOOTER"
  , C.Assign 788 "right" (Num 0)
  , C.Seq 788 789
  , C.Var 789 "left_o"
  , C.Seq 789 790
  , C.Var 790 "right_o"
  , C.Seq 790 791
  , C.Var 791 "dres"
  , C.Seq 791 792
  , C.Assign 792 "res" (Num 0)
  , C.Seq 792 793
  , C.Branch 793 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 795 795
  , C.Seq 794 1813
  , C.Seq 794 796
  , C.Var 795 "NOP_795"
  , C.Seq 795 796
  , C.Var 796 "IF_ELSE_FOOTER"
  , C.Assign 797 "undefed" (Num 0)
  , C.Seq 797 798
  , C.Assign 798 "stack_pointer" (Num 0)
  , C.Seq 798 799
  , C.Var 799 "word"
  , C.Seq 799 800
  , C.Assign 800 "opcode" (Num 0)
  , C.Seq 800 801
  , C.Assign 801 "oparg" (Num 0)
  , C.Seq 801 802
  , C.Branch 802 (Eq (Num 0) (Num 1)) 803 806
  , C.Var 803 "word"
  , C.Seq 803 804
  , C.Assign 804 "opcode" (Num 0)
  , C.Seq 804 805
  , C.Assign 805 "oparg" (Num 0)
  , C.Seq 805 806
  , C.Seq 805 802
  , C.Var 806 "LOOP_FOOTER"
  , C.Seq 806 807
  , C.Seq 806 35
  , C.Branch 807 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 809 848
  , C.Var 809 "NOP_809"
  , C.Var 810 "__CLABEL_TARGET_BINARY_OP_SUBTRACT_INT"
  , C.Seq 810 811
  , C.Var 811 "this_instr"
  , C.Seq 811 812
  , C.Assign 812 "undefed" (Num 0)
  , C.Seq 812 813
  , C.Assign 813 "next_instr" (Num 0)
  , C.Seq 813 814
  , C.Var 814 "value"
  , C.Seq 814 815
  , C.Var 815 "left"
  , C.Seq 815 816
  , C.Var 816 "right"
  , C.Seq 816 817
  , C.Var 817 "res"
  , C.Seq 817 818
  , C.Assign 818 "value" (Num 0)
  , C.Seq 818 819
  , C.Var 819 "value_o"
  , C.Seq 819 820
  , C.Branch 820 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 822 822
  , C.Seq 821 42
  , C.Seq 821 823
  , C.Var 822 "NOP_822"
  , C.Seq 822 823
  , C.Var 823 "IF_ELSE_FOOTER"
  , C.Assign 824 "left" (Num 0)
  , C.Seq 824 825
  , C.Var 825 "left_o"
  , C.Seq 825 826
  , C.Branch 826 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 828 828
  , C.Seq 827 42
  , C.Seq 827 829
  , C.Var 828 "NOP_828"
  , C.Seq 828 829
  , C.Var 829 "IF_ELSE_FOOTER"
  , C.Assign 830 "right" (Num 0)
  , C.Seq 830 831
  , C.Var 831 "left_o"
  , C.Seq 831 832
  , C.Var 832 "right_o"
  , C.Seq 832 833
  , C.Assign 833 "res" (Num 0)
  , C.Seq 833 834
  , C.Branch 834 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 836 836
  , C.Seq 835 42
  , C.Seq 835 837
  , C.Var 836 "NOP_836"
  , C.Seq 836 837
  , C.Var 837 "IF_ELSE_FOOTER"
  , C.Assign 838 "undefed" (Num 0)
  , C.Seq 838 839
  , C.Assign 839 "stack_pointer" (Num 0)
  , C.Seq 839 840
  , C.Var 840 "word"
  , C.Seq 840 841
  , C.Assign 841 "opcode" (Num 0)
  , C.Seq 841 842
  , C.Assign 842 "oparg" (Num 0)
  , C.Seq 842 843
  , C.Branch 843 (Eq (Num 0) (Num 1)) 844 847
  , C.Var 844 "word"
  , C.Seq 844 845
  , C.Assign 845 "opcode" (Num 0)
  , C.Seq 845 846
  , C.Assign 846 "oparg" (Num 0)
  , C.Seq 846 847
  , C.Seq 846 843
  , C.Var 847 "LOOP_FOOTER"
  , C.Seq 847 848
  , C.Seq 847 35
  , C.Branch 848 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 850 931
  , C.Var 850 "NOP_850"
  , C.Var 851 "__CLABEL_TARGET_BINARY_SLICE"
  , C.Seq 851 852
  , C.Assign 852 "undefed" (Num 0)
  , C.Seq 852 853
  , C.Assign 853 "next_instr" (Num 0)
  , C.Seq 853 854
  , C.Var 854 "container"
  , C.Seq 854 855
  , C.Var 855 "start"
  , C.Seq 855 856
  , C.Var 856 "stop"
  , C.Seq 856 857
  , C.Var 857 "res"
  , C.Seq 857 858
  , C.Assign 858 "stop" (Num 0)
  , C.Seq 858 859
  , C.Assign 859 "start" (Num 0)
  , C.Seq 859 860
  , C.Assign 860 "container" (Num 0)
  , C.Seq 860 861
  , C.Var 861 "slice"
  , C.Seq 861 862
  , C.Assign 862 "stack_pointer" (Num 0)
  , C.Seq 862 863
  , C.Var 863 "res_o"
  , C.Seq 863 864
  , C.Branch 864 (Eq (Plus (Id "slice") (Num 0)) (Num 1)) 866 867
  , C.Assign 866 "res_o" (Num 0)
  , C.Seq 866 913
  , C.Assign 867 "stack_pointer" (Num 0)
  , C.Seq 867 868
  , C.Assign 868 "res_o" (Num 0)
  , C.Seq 868 869
  , C.Var 869 "op"
  , C.Seq 869 870
  , C.Branch 870 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 872 887
  , C.Var 872 "tracer"
  , C.Seq 872 873
  , C.Branch 873 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 875 876
  , C.Var 875 "data"
  , C.Seq 875 876
  , C.Seq 875 877
  , C.Var 876 "NOP_876"
  , C.Seq 876 877
  , C.Var 877 "IF_ELSE_FOOTER"
  , C.Branch 878 (Eq (Num 0) (Num 1)) 879 885
  , C.Var 879 "tracer"
  , C.Seq 879 880
  , C.Branch 880 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 882 883
  , C.Var 882 "data"
  , C.Seq 882 883
  , C.Seq 882 884
  , C.Var 883 "NOP_883"
  , C.Seq 883 884
  , C.Var 884 "IF_ELSE_FOOTER"
  , C.Seq 884 878
  , C.Var 885 "LOOP_FOOTER"
  , C.Seq 885 886
  , C.Var 886 "dealloc"
  , C.Seq 886 887
  , C.Seq 886 888
  , C.Var 887 "NOP_887"
  , C.Seq 887 888
  , C.Var 888 "IF_ELSE_FOOTER"
  , C.Branch 889 (Eq (Num 0) (Num 1)) 890 910
  , C.Var 890 "op"
  , C.Seq 890 891
  , C.Branch 891 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 893 908
  , C.Var 893 "tracer"
  , C.Seq 893 894
  , C.Branch 894 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 896 897
  , C.Var 896 "data"
  , C.Seq 896 897
  , C.Seq 896 898
  , C.Var 897 "NOP_897"
  , C.Seq 897 898
  , C.Var 898 "IF_ELSE_FOOTER"
  , C.Branch 899 (Eq (Num 0) (Num 1)) 900 906
  , C.Var 900 "tracer"
  , C.Seq 900 901
  , C.Branch 901 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 903 904
  , C.Var 903 "data"
  , C.Seq 903 904
  , C.Seq 903 905
  , C.Var 904 "NOP_904"
  , C.Seq 904 905
  , C.Var 905 "IF_ELSE_FOOTER"
  , C.Seq 905 899
  , C.Var 906 "LOOP_FOOTER"
  , C.Seq 906 907
  , C.Var 907 "dealloc"
  , C.Seq 907 908
  , C.Seq 907 909
  , C.Var 908 "NOP_908"
  , C.Seq 908 909
  , C.Var 909 "IF_ELSE_FOOTER"
  , C.Seq 909 889
  , C.Var 910 "LOOP_FOOTER"
  , C.Seq 910 911
  , C.Assign 911 "stack_pointer" (Num 0)
  , C.Seq 911 912
  , C.Assign 912 "stack_pointer" (Num 0)
  , C.Seq 912 913
  , C.Var 913 "IF_ELSE_FOOTER"
  , C.Assign 914 "stack_pointer" (Num 0)
  , C.Seq 914 915
  , C.Assign 915 "stack_pointer" (Num 0)
  , C.Seq 915 916
  , C.Branch 916 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 918 918
  , C.Seq 917 1817
  , C.Seq 917 919
  , C.Var 918 "NOP_918"
  , C.Seq 918 919
  , C.Var 919 "IF_ELSE_FOOTER"
  , C.Assign 920 "res" (Num 0)
  , C.Seq 920 921
  , C.Assign 921 "undefed" (Num 0)
  , C.Seq 921 922
  , C.Assign 922 "stack_pointer" (Num 0)
  , C.Seq 922 923
  , C.Var 923 "word"
  , C.Seq 923 924
  , C.Assign 924 "opcode" (Num 0)
  , C.Seq 924 925
  , C.Assign 925 "oparg" (Num 0)
  , C.Seq 925 926
  , C.Branch 926 (Eq (Num 0) (Num 1)) 927 930
  , C.Var 927 "word"
  , C.Seq 927 928
  , C.Assign 928 "opcode" (Num 0)
  , C.Seq 928 929
  , C.Assign 929 "oparg" (Num 0)
  , C.Seq 929 930
  , C.Seq 929 926
  , C.Var 930 "LOOP_FOOTER"
  , C.Seq 930 931
  , C.Seq 930 35
  , C.Branch 931 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 933 980
  , C.Var 933 "NOP_933"
  , C.Var 934 "__CLABEL_TARGET_BUILD_INTERPOLATION"
  , C.Seq 934 935
  , C.Assign 935 "undefed" (Num 0)
  , C.Seq 935 936
  , C.Assign 936 "next_instr" (Num 0)
  , C.Seq 936 937
  , C.Var 937 "value"
  , C.Seq 937 938
  , C.Var 938 "str"
  , C.Seq 938 939
  , C.Var 939 "format"
  , C.Seq 939 940
  , C.Var 940 "interpolation"
  , C.Seq 940 941
  , C.Assign 941 "format" (Num 0)
  , C.Seq 941 942
  , C.Assign 942 "str" (Num 0)
  , C.Seq 942 943
  , C.Assign 943 "value" (Num 0)
  , C.Seq 943 944
  , C.Var 944 "value_o"
  , C.Seq 944 945
  , C.Var 945 "str_o"
  , C.Seq 945 946
  , C.Var 946 "conversion"
  , C.Seq 946 947
  , C.Var 947 "format_o"
  , C.Seq 947 948
  , C.Branch 948 (Eq (Plus (Id "oparg") (Num 0)) (Num 1)) 950 951
  , C.Assign 950 "format_o" (Num 0)
  , C.Seq 950 952
  , C.Assign 951 "format_o" (Num 0)
  , C.Seq 951 952
  , C.Var 952 "IF_ELSE_FOOTER"
  , C.Var 953 "interpolation_o"
  , C.Seq 953 954
  , C.Assign 954 "stack_pointer" (Num 0)
  , C.Seq 954 955
  , C.Branch 955 (Eq (Plus (Id "oparg") (Num 0)) (Num 1)) 957 959
  , C.Assign 957 "stack_pointer" (Num 0)
  , C.Seq 957 958
  , C.Assign 958 "stack_pointer" (Num 0)
  , C.Seq 958 959
  , C.Seq 958 960
  , C.Assign 959 "stack_pointer" (Num 0)
  , C.Seq 959 960
  , C.Var 960 "IF_ELSE_FOOTER"
  , C.Assign 961 "stack_pointer" (Num 0)
  , C.Seq 961 962
  , C.Assign 962 "stack_pointer" (Num 0)
  , C.Seq 962 963
  , C.Assign 963 "stack_pointer" (Num 0)
  , C.Seq 963 964
  , C.Assign 964 "stack_pointer" (Num 0)
  , C.Seq 964 965
  , C.Branch 965 (Eq (Plus (Id "interpolation_o") (Num 0)) (Num 1)) 967 967
  , C.Seq 966 1817
  , C.Seq 966 968
  , C.Var 967 "NOP_967"
  , C.Seq 967 968
  , C.Var 968 "IF_ELSE_FOOTER"
  , C.Assign 969 "interpolation" (Num 0)
  , C.Seq 969 970
  , C.Assign 970 "undefed" (Num 0)
  , C.Seq 970 971
  , C.Assign 971 "stack_pointer" (Num 0)
  , C.Seq 971 972
  , C.Var 972 "word"
  , C.Seq 972 973
  , C.Assign 973 "opcode" (Num 0)
  , C.Seq 973 974
  , C.Assign 974 "oparg" (Num 0)
  , C.Seq 974 975
  , C.Branch 975 (Eq (Num 0) (Num 1)) 976 979
  , C.Var 976 "word"
  , C.Seq 976 977
  , C.Assign 977 "opcode" (Num 0)
  , C.Seq 977 978
  , C.Assign 978 "oparg" (Num 0)
  , C.Seq 978 979
  , C.Seq 978 975
  , C.Var 979 "LOOP_FOOTER"
  , C.Seq 979 980
  , C.Seq 979 35
  , C.Branch 980 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 982 1006
  , C.Var 982 "NOP_982"
  , C.Var 983 "__CLABEL_TARGET_BUILD_LIST"
  , C.Seq 983 984
  , C.Assign 984 "undefed" (Num 0)
  , C.Seq 984 985
  , C.Assign 985 "next_instr" (Num 0)
  , C.Seq 985 986
  , C.Var 986 "values"
  , C.Seq 986 987
  , C.Var 987 "list"
  , C.Seq 987 988
  , C.Assign 988 "values" (Num 0)
  , C.Seq 988 989
  , C.Var 989 "list_o"
  , C.Seq 989 990
  , C.Assign 990 "stack_pointer" (Num 0)
  , C.Seq 990 991
  , C.Branch 991 (Eq (Plus (Id "list_o") (Num 0)) (Num 1)) 993 993
  , C.Seq 992 1817
  , C.Seq 992 994
  , C.Var 993 "NOP_993"
  , C.Seq 993 994
  , C.Var 994 "IF_ELSE_FOOTER"
  , C.Assign 995 "list" (Num 0)
  , C.Seq 995 996
  , C.Assign 996 "undefed" (Num 0)
  , C.Seq 996 997
  , C.Assign 997 "stack_pointer" (Num 0)
  , C.Seq 997 998
  , C.Var 998 "word"
  , C.Seq 998 999
  , C.Assign 999 "opcode" (Num 0)
  , C.Seq 999 1000
  , C.Assign 1000 "oparg" (Num 0)
  , C.Seq 1000 1001
  , C.Branch 1001 (Eq (Num 0) (Num 1)) 1002 1005
  , C.Var 1002 "word"
  , C.Seq 1002 1003
  , C.Assign 1003 "opcode" (Num 0)
  , C.Seq 1003 1004
  , C.Assign 1004 "oparg" (Num 0)
  , C.Seq 1004 1005
  , C.Seq 1004 1001
  , C.Var 1005 "LOOP_FOOTER"
  , C.Seq 1005 1006
  , C.Seq 1005 35
  , C.Branch 1006 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1008 1054
  , C.Var 1008 "NOP_1008"
  , C.Var 1009 "__CLABEL_TARGET_BUILD_MAP"
  , C.Seq 1009 1010
  , C.Assign 1010 "undefed" (Num 0)
  , C.Seq 1010 1011
  , C.Assign 1011 "next_instr" (Num 0)
  , C.Seq 1011 1012
  , C.Var 1012 "values"
  , C.Seq 1012 1013
  , C.Var 1013 "map"
  , C.Seq 1013 1014
  , C.Assign 1014 "values" (Num 0)
  , C.Seq 1014 1015
  , C.Var 1015 "values_o_temp"
  , C.Seq 1015 1016
  , C.Var 1016 "values_o"
  , C.Seq 1016 1017
  , C.Branch 1017 (Eq (Plus (Id "values_o") (Num 0)) (Num 1)) 1019 1027
  , C.Var 1019 "tmp"
  , C.Seq 1019 1020
  , C.Var 1020 "_i"
  , C.Seq 1020 1021
  , C.Branch 1021 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1022 1024
  , C.Assign 1022 "tmp" (Num 0)
  , C.Seq 1022 1023
  , C.Assign 1023 "undefed" (Num 0)
  , C.Seq 1023 1024
  , C.Seq 1023 1021
  , C.Var 1024 "LOOP_FOOTER"
  , C.Seq 1024 1025
  , C.Assign 1025 "stack_pointer" (Num 0)
  , C.Seq 1025 1026
  , C.Assign 1026 "stack_pointer" (Num 0)
  , C.Seq 1026 1027
  , C.Seq 1026 1817
  , C.Seq 1026 1028
  , C.Var 1027 "NOP_1027"
  , C.Seq 1027 1028
  , C.Var 1028 "IF_ELSE_FOOTER"
  , C.Var 1029 "map_o"
  , C.Seq 1029 1030
  , C.Assign 1030 "stack_pointer" (Num 0)
  , C.Seq 1030 1031
  , C.Var 1031 "tmp"
  , C.Seq 1031 1032
  , C.Var 1032 "_i"
  , C.Seq 1032 1033
  , C.Branch 1033 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1034 1036
  , C.Assign 1034 "tmp" (Num 0)
  , C.Seq 1034 1035
  , C.Assign 1035 "undefed" (Num 0)
  , C.Seq 1035 1036
  , C.Seq 1035 1033
  , C.Var 1036 "LOOP_FOOTER"
  , C.Seq 1036 1037
  , C.Assign 1037 "stack_pointer" (Num 0)
  , C.Seq 1037 1038
  , C.Assign 1038 "stack_pointer" (Num 0)
  , C.Seq 1038 1039
  , C.Branch 1039 (Eq (Plus (Id "map_o") (Num 0)) (Num 1)) 1041 1041
  , C.Seq 1040 1817
  , C.Seq 1040 1042
  , C.Var 1041 "NOP_1041"
  , C.Seq 1041 1042
  , C.Var 1042 "IF_ELSE_FOOTER"
  , C.Assign 1043 "map" (Num 0)
  , C.Seq 1043 1044
  , C.Assign 1044 "undefed" (Num 0)
  , C.Seq 1044 1045
  , C.Assign 1045 "stack_pointer" (Num 0)
  , C.Seq 1045 1046
  , C.Var 1046 "word"
  , C.Seq 1046 1047
  , C.Assign 1047 "opcode" (Num 0)
  , C.Seq 1047 1048
  , C.Assign 1048 "oparg" (Num 0)
  , C.Seq 1048 1049
  , C.Branch 1049 (Eq (Num 0) (Num 1)) 1050 1053
  , C.Var 1050 "word"
  , C.Seq 1050 1051
  , C.Assign 1051 "opcode" (Num 0)
  , C.Seq 1051 1052
  , C.Assign 1052 "oparg" (Num 0)
  , C.Seq 1052 1053
  , C.Seq 1052 1049
  , C.Var 1053 "LOOP_FOOTER"
  , C.Seq 1053 1054
  , C.Seq 1053 35
  , C.Branch 1054 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1056 1148
  , C.Var 1056 "NOP_1056"
  , C.Var 1057 "__CLABEL_TARGET_BUILD_SET"
  , C.Seq 1057 1058
  , C.Assign 1058 "undefed" (Num 0)
  , C.Seq 1058 1059
  , C.Assign 1059 "next_instr" (Num 0)
  , C.Seq 1059 1060
  , C.Var 1060 "values"
  , C.Seq 1060 1061
  , C.Var 1061 "set"
  , C.Seq 1061 1062
  , C.Assign 1062 "values" (Num 0)
  , C.Seq 1062 1063
  , C.Var 1063 "set_o"
  , C.Seq 1063 1064
  , C.Assign 1064 "stack_pointer" (Num 0)
  , C.Seq 1064 1065
  , C.Branch 1065 (Eq (Plus (Id "set_o") (Num 0)) (Num 1)) 1067 1075
  , C.Var 1067 "tmp"
  , C.Seq 1067 1068
  , C.Var 1068 "_i"
  , C.Seq 1068 1069
  , C.Branch 1069 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1070 1072
  , C.Assign 1070 "tmp" (Num 0)
  , C.Seq 1070 1071
  , C.Assign 1071 "undefed" (Num 0)
  , C.Seq 1071 1072
  , C.Seq 1071 1069
  , C.Var 1072 "LOOP_FOOTER"
  , C.Seq 1072 1073
  , C.Assign 1073 "stack_pointer" (Num 0)
  , C.Seq 1073 1074
  , C.Assign 1074 "stack_pointer" (Num 0)
  , C.Seq 1074 1075
  , C.Seq 1074 1817
  , C.Seq 1074 1076
  , C.Var 1075 "NOP_1075"
  , C.Seq 1075 1076
  , C.Var 1076 "IF_ELSE_FOOTER"
  , C.Var 1077 "err"
  , C.Seq 1077 1078
  , C.Var 1078 "i"
  , C.Seq 1078 1079
  , C.Branch 1079 (Eq (Plus (Id "i") (Id "oparg")) (Num 1)) 1080 1088
  , C.Var 1080 "value"
  , C.Seq 1080 1081
  , C.Assign 1081 "undefed" (Num 0)
  , C.Seq 1081 1082
  , C.Branch 1082 (Eq (Plus (Id "err") (Num 0)) (Num 1)) 1084 1086
  , C.Assign 1084 "err" (Num 0)
  , C.Seq 1084 1085
  , C.Assign 1085 "stack_pointer" (Num 0)
  , C.Seq 1085 1086
  , C.Seq 1085 1087
  , C.Assign 1086 "stack_pointer" (Num 0)
  , C.Seq 1086 1087
  , C.Var 1087 "IF_ELSE_FOOTER"
  , C.Seq 1087 1079
  , C.Var 1088 "LOOP_FOOTER"
  , C.Seq 1088 1089
  , C.Branch 1089 (Eq (Id "err") (Num 1)) 1091 1135
  , C.Assign 1091 "stack_pointer" (Num 0)
  , C.Seq 1091 1092
  , C.Var 1092 "op"
  , C.Seq 1092 1093
  , C.Branch 1093 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1095 1110
  , C.Var 1095 "tracer"
  , C.Seq 1095 1096
  , C.Branch 1096 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1098 1099
  , C.Var 1098 "data"
  , C.Seq 1098 1099
  , C.Seq 1098 1100
  , C.Var 1099 "NOP_1099"
  , C.Seq 1099 1100
  , C.Var 1100 "IF_ELSE_FOOTER"
  , C.Branch 1101 (Eq (Num 0) (Num 1)) 1102 1108
  , C.Var 1102 "tracer"
  , C.Seq 1102 1103
  , C.Branch 1103 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1105 1106
  , C.Var 1105 "data"
  , C.Seq 1105 1106
  , C.Seq 1105 1107
  , C.Var 1106 "NOP_1106"
  , C.Seq 1106 1107
  , C.Var 1107 "IF_ELSE_FOOTER"
  , C.Seq 1107 1101
  , C.Var 1108 "LOOP_FOOTER"
  , C.Seq 1108 1109
  , C.Var 1109 "dealloc"
  , C.Seq 1109 1110
  , C.Seq 1109 1111
  , C.Var 1110 "NOP_1110"
  , C.Seq 1110 1111
  , C.Var 1111 "IF_ELSE_FOOTER"
  , C.Branch 1112 (Eq (Num 0) (Num 1)) 1113 1133
  , C.Var 1113 "op"
  , C.Seq 1113 1114
  , C.Branch 1114 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1116 1131
  , C.Var 1116 "tracer"
  , C.Seq 1116 1117
  , C.Branch 1117 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1119 1120
  , C.Var 1119 "data"
  , C.Seq 1119 1120
  , C.Seq 1119 1121
  , C.Var 1120 "NOP_1120"
  , C.Seq 1120 1121
  , C.Var 1121 "IF_ELSE_FOOTER"
  , C.Branch 1122 (Eq (Num 0) (Num 1)) 1123 1129
  , C.Var 1123 "tracer"
  , C.Seq 1123 1124
  , C.Branch 1124 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1126 1127
  , C.Var 1126 "data"
  , C.Seq 1126 1127
  , C.Seq 1126 1128
  , C.Var 1127 "NOP_1127"
  , C.Seq 1127 1128
  , C.Var 1128 "IF_ELSE_FOOTER"
  , C.Seq 1128 1122
  , C.Var 1129 "LOOP_FOOTER"
  , C.Seq 1129 1130
  , C.Var 1130 "dealloc"
  , C.Seq 1130 1131
  , C.Seq 1130 1132
  , C.Var 1131 "NOP_1131"
  , C.Seq 1131 1132
  , C.Var 1132 "IF_ELSE_FOOTER"
  , C.Seq 1132 1112
  , C.Var 1133 "LOOP_FOOTER"
  , C.Seq 1133 1134
  , C.Assign 1134 "stack_pointer" (Num 0)
  , C.Seq 1134 1135
  , C.Seq 1134 1817
  , C.Seq 1134 1136
  , C.Var 1135 "NOP_1135"
  , C.Seq 1135 1136
  , C.Var 1136 "IF_ELSE_FOOTER"
  , C.Assign 1137 "set" (Num 0)
  , C.Seq 1137 1138
  , C.Assign 1138 "undefed" (Num 0)
  , C.Seq 1138 1139
  , C.Assign 1139 "stack_pointer" (Num 0)
  , C.Seq 1139 1140
  , C.Var 1140 "word"
  , C.Seq 1140 1141
  , C.Assign 1141 "opcode" (Num 0)
  , C.Seq 1141 1142
  , C.Assign 1142 "oparg" (Num 0)
  , C.Seq 1142 1143
  , C.Branch 1143 (Eq (Num 0) (Num 1)) 1144 1147
  , C.Var 1144 "word"
  , C.Seq 1144 1145
  , C.Assign 1145 "opcode" (Num 0)
  , C.Seq 1145 1146
  , C.Assign 1146 "oparg" (Num 0)
  , C.Seq 1146 1147
  , C.Seq 1146 1143
  , C.Var 1147 "LOOP_FOOTER"
  , C.Seq 1147 1148
  , C.Seq 1147 35
  , C.Branch 1148 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1150 1184
  , C.Var 1150 "NOP_1150"
  , C.Var 1151 "__CLABEL_TARGET_BUILD_SLICE"
  , C.Seq 1151 1152
  , C.Assign 1152 "undefed" (Num 0)
  , C.Seq 1152 1153
  , C.Assign 1153 "next_instr" (Num 0)
  , C.Seq 1153 1154
  , C.Var 1154 "args"
  , C.Seq 1154 1155
  , C.Var 1155 "slice"
  , C.Seq 1155 1156
  , C.Assign 1156 "args" (Num 0)
  , C.Seq 1156 1157
  , C.Var 1157 "start_o"
  , C.Seq 1157 1158
  , C.Var 1158 "stop_o"
  , C.Seq 1158 1159
  , C.Var 1159 "step_o"
  , C.Seq 1159 1160
  , C.Var 1160 "slice_o"
  , C.Seq 1160 1161
  , C.Var 1161 "tmp"
  , C.Seq 1161 1162
  , C.Var 1162 "_i"
  , C.Seq 1162 1163
  , C.Branch 1163 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1164 1166
  , C.Assign 1164 "tmp" (Num 0)
  , C.Seq 1164 1165
  , C.Assign 1165 "undefed" (Num 0)
  , C.Seq 1165 1166
  , C.Seq 1165 1163
  , C.Var 1166 "LOOP_FOOTER"
  , C.Seq 1166 1167
  , C.Assign 1167 "stack_pointer" (Num 0)
  , C.Seq 1167 1168
  , C.Assign 1168 "stack_pointer" (Num 0)
  , C.Seq 1168 1169
  , C.Branch 1169 (Eq (Plus (Id "slice_o") (Num 0)) (Num 1)) 1171 1171
  , C.Seq 1170 1817
  , C.Seq 1170 1172
  , C.Var 1171 "NOP_1171"
  , C.Seq 1171 1172
  , C.Var 1172 "IF_ELSE_FOOTER"
  , C.Assign 1173 "slice" (Num 0)
  , C.Seq 1173 1174
  , C.Assign 1174 "undefed" (Num 0)
  , C.Seq 1174 1175
  , C.Assign 1175 "stack_pointer" (Num 0)
  , C.Seq 1175 1176
  , C.Var 1176 "word"
  , C.Seq 1176 1177
  , C.Assign 1177 "opcode" (Num 0)
  , C.Seq 1177 1178
  , C.Assign 1178 "oparg" (Num 0)
  , C.Seq 1178 1179
  , C.Branch 1179 (Eq (Num 0) (Num 1)) 1180 1183
  , C.Var 1180 "word"
  , C.Seq 1180 1181
  , C.Assign 1181 "opcode" (Num 0)
  , C.Seq 1181 1182
  , C.Assign 1182 "oparg" (Num 0)
  , C.Seq 1182 1183
  , C.Seq 1182 1179
  , C.Var 1183 "LOOP_FOOTER"
  , C.Seq 1183 1184
  , C.Seq 1183 35
  , C.Branch 1184 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1186 1231
  , C.Var 1186 "NOP_1186"
  , C.Var 1187 "__CLABEL_TARGET_BUILD_STRING"
  , C.Seq 1187 1188
  , C.Assign 1188 "undefed" (Num 0)
  , C.Seq 1188 1189
  , C.Assign 1189 "next_instr" (Num 0)
  , C.Seq 1189 1190
  , C.Var 1190 "pieces"
  , C.Seq 1190 1191
  , C.Var 1191 "str"
  , C.Seq 1191 1192
  , C.Assign 1192 "pieces" (Num 0)
  , C.Seq 1192 1193
  , C.Var 1193 "pieces_o_temp"
  , C.Seq 1193 1194
  , C.Var 1194 "pieces_o"
  , C.Seq 1194 1195
  , C.Branch 1195 (Eq (Plus (Id "pieces_o") (Num 0)) (Num 1)) 1197 1205
  , C.Var 1197 "tmp"
  , C.Seq 1197 1198
  , C.Var 1198 "_i"
  , C.Seq 1198 1199
  , C.Branch 1199 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1200 1202
  , C.Assign 1200 "tmp" (Num 0)
  , C.Seq 1200 1201
  , C.Assign 1201 "undefed" (Num 0)
  , C.Seq 1201 1202
  , C.Seq 1201 1199
  , C.Var 1202 "LOOP_FOOTER"
  , C.Seq 1202 1203
  , C.Assign 1203 "stack_pointer" (Num 0)
  , C.Seq 1203 1204
  , C.Assign 1204 "stack_pointer" (Num 0)
  , C.Seq 1204 1205
  , C.Seq 1204 1817
  , C.Seq 1204 1206
  , C.Var 1205 "NOP_1205"
  , C.Seq 1205 1206
  , C.Var 1206 "IF_ELSE_FOOTER"
  , C.Var 1207 "str_o"
  , C.Seq 1207 1208
  , C.Var 1208 "tmp"
  , C.Seq 1208 1209
  , C.Var 1209 "_i"
  , C.Seq 1209 1210
  , C.Branch 1210 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1211 1213
  , C.Assign 1211 "tmp" (Num 0)
  , C.Seq 1211 1212
  , C.Assign 1212 "undefed" (Num 0)
  , C.Seq 1212 1213
  , C.Seq 1212 1210
  , C.Var 1213 "LOOP_FOOTER"
  , C.Seq 1213 1214
  , C.Assign 1214 "stack_pointer" (Num 0)
  , C.Seq 1214 1215
  , C.Assign 1215 "stack_pointer" (Num 0)
  , C.Seq 1215 1216
  , C.Branch 1216 (Eq (Plus (Id "str_o") (Num 0)) (Num 1)) 1218 1218
  , C.Seq 1217 1817
  , C.Seq 1217 1219
  , C.Var 1218 "NOP_1218"
  , C.Seq 1218 1219
  , C.Var 1219 "IF_ELSE_FOOTER"
  , C.Assign 1220 "str" (Num 0)
  , C.Seq 1220 1221
  , C.Assign 1221 "undefed" (Num 0)
  , C.Seq 1221 1222
  , C.Assign 1222 "stack_pointer" (Num 0)
  , C.Seq 1222 1223
  , C.Var 1223 "word"
  , C.Seq 1223 1224
  , C.Assign 1224 "opcode" (Num 0)
  , C.Seq 1224 1225
  , C.Assign 1225 "oparg" (Num 0)
  , C.Seq 1225 1226
  , C.Branch 1226 (Eq (Num 0) (Num 1)) 1227 1230
  , C.Var 1227 "word"
  , C.Seq 1227 1228
  , C.Assign 1228 "opcode" (Num 0)
  , C.Seq 1228 1229
  , C.Assign 1229 "oparg" (Num 0)
  , C.Seq 1229 1230
  , C.Seq 1229 1226
  , C.Var 1230 "LOOP_FOOTER"
  , C.Seq 1230 1231
  , C.Seq 1230 35
  , C.Branch 1231 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1233 1265
  , C.Var 1233 "NOP_1233"
  , C.Var 1234 "__CLABEL_TARGET_BUILD_TEMPLATE"
  , C.Seq 1234 1235
  , C.Assign 1235 "undefed" (Num 0)
  , C.Seq 1235 1236
  , C.Assign 1236 "next_instr" (Num 0)
  , C.Seq 1236 1237
  , C.Var 1237 "strings"
  , C.Seq 1237 1238
  , C.Var 1238 "interpolations"
  , C.Seq 1238 1239
  , C.Var 1239 "template"
  , C.Seq 1239 1240
  , C.Assign 1240 "interpolations" (Num 0)
  , C.Seq 1240 1241
  , C.Assign 1241 "strings" (Num 0)
  , C.Seq 1241 1242
  , C.Var 1242 "strings_o"
  , C.Seq 1242 1243
  , C.Var 1243 "interpolations_o"
  , C.Seq 1243 1244
  , C.Var 1244 "template_o"
  , C.Seq 1244 1245
  , C.Assign 1245 "stack_pointer" (Num 0)
  , C.Seq 1245 1246
  , C.Assign 1246 "stack_pointer" (Num 0)
  , C.Seq 1246 1247
  , C.Assign 1247 "stack_pointer" (Num 0)
  , C.Seq 1247 1248
  , C.Assign 1248 "stack_pointer" (Num 0)
  , C.Seq 1248 1249
  , C.Assign 1249 "stack_pointer" (Num 0)
  , C.Seq 1249 1250
  , C.Branch 1250 (Eq (Plus (Id "template_o") (Num 0)) (Num 1)) 1252 1252
  , C.Seq 1251 1817
  , C.Seq 1251 1253
  , C.Var 1252 "NOP_1252"
  , C.Seq 1252 1253
  , C.Var 1253 "IF_ELSE_FOOTER"
  , C.Assign 1254 "template" (Num 0)
  , C.Seq 1254 1255
  , C.Assign 1255 "undefed" (Num 0)
  , C.Seq 1255 1256
  , C.Assign 1256 "stack_pointer" (Num 0)
  , C.Seq 1256 1257
  , C.Var 1257 "word"
  , C.Seq 1257 1258
  , C.Assign 1258 "opcode" (Num 0)
  , C.Seq 1258 1259
  , C.Assign 1259 "oparg" (Num 0)
  , C.Seq 1259 1260
  , C.Branch 1260 (Eq (Num 0) (Num 1)) 1261 1264
  , C.Var 1261 "word"
  , C.Seq 1261 1262
  , C.Assign 1262 "opcode" (Num 0)
  , C.Seq 1262 1263
  , C.Assign 1263 "oparg" (Num 0)
  , C.Seq 1263 1264
  , C.Seq 1263 1260
  , C.Var 1264 "LOOP_FOOTER"
  , C.Seq 1264 1265
  , C.Seq 1264 35
  , C.Branch 1265 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1267 1290
  , C.Var 1267 "NOP_1267"
  , C.Var 1268 "__CLABEL_TARGET_BUILD_TUPLE"
  , C.Seq 1268 1269
  , C.Assign 1269 "undefed" (Num 0)
  , C.Seq 1269 1270
  , C.Assign 1270 "next_instr" (Num 0)
  , C.Seq 1270 1271
  , C.Var 1271 "values"
  , C.Seq 1271 1272
  , C.Var 1272 "tup"
  , C.Seq 1272 1273
  , C.Assign 1273 "values" (Num 0)
  , C.Seq 1273 1274
  , C.Var 1274 "tup_o"
  , C.Seq 1274 1275
  , C.Branch 1275 (Eq (Plus (Id "tup_o") (Num 0)) (Num 1)) 1277 1277
  , C.Seq 1276 1817
  , C.Seq 1276 1278
  , C.Var 1277 "NOP_1277"
  , C.Seq 1277 1278
  , C.Var 1278 "IF_ELSE_FOOTER"
  , C.Assign 1279 "tup" (Num 0)
  , C.Seq 1279 1280
  , C.Assign 1280 "undefed" (Num 0)
  , C.Seq 1280 1281
  , C.Assign 1281 "stack_pointer" (Num 0)
  , C.Seq 1281 1282
  , C.Var 1282 "word"
  , C.Seq 1282 1283
  , C.Assign 1283 "opcode" (Num 0)
  , C.Seq 1283 1284
  , C.Assign 1284 "oparg" (Num 0)
  , C.Seq 1284 1285
  , C.Branch 1285 (Eq (Num 0) (Num 1)) 1286 1289
  , C.Var 1286 "word"
  , C.Seq 1286 1287
  , C.Assign 1287 "opcode" (Num 0)
  , C.Seq 1287 1288
  , C.Assign 1288 "oparg" (Num 0)
  , C.Seq 1288 1289
  , C.Seq 1288 1285
  , C.Var 1289 "LOOP_FOOTER"
  , C.Seq 1289 1290
  , C.Seq 1289 35
  , C.Branch 1290 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1292 1304
  , C.Var 1292 "NOP_1292"
  , C.Var 1293 "__CLABEL_TARGET_CACHE"
  , C.Seq 1293 1294
  , C.Assign 1294 "undefed" (Num 0)
  , C.Seq 1294 1295
  , C.Assign 1295 "next_instr" (Num 0)
  , C.Seq 1295 1296
  , C.Var 1296 "word"
  , C.Seq 1296 1297
  , C.Assign 1297 "opcode" (Num 0)
  , C.Seq 1297 1298
  , C.Assign 1298 "oparg" (Num 0)
  , C.Seq 1298 1299
  , C.Branch 1299 (Eq (Num 0) (Num 1)) 1300 1303
  , C.Var 1300 "word"
  , C.Seq 1300 1301
  , C.Assign 1301 "opcode" (Num 0)
  , C.Seq 1301 1302
  , C.Assign 1302 "oparg" (Num 0)
  , C.Seq 1302 1303
  , C.Seq 1302 1299
  , C.Var 1303 "LOOP_FOOTER"
  , C.Seq 1303 1304
  , C.Seq 1303 35
  , C.Branch 1304 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1306 1551
  , C.Var 1306 "NOP_1306"
  , C.Var 1307 "__CLABEL_TARGET_CALL"
  , C.Seq 1307 1308
  , C.Assign 1308 "undefed" (Num 0)
  , C.Seq 1308 1309
  , C.Assign 1309 "next_instr" (Num 0)
  , C.Seq 1309 1310
  , C.Var 1310 "__CLABEL_PREDICTED_CALL"
  , C.Seq 1310 1311
  , C.Var 1311 "NOP_1311"
  , C.Var 1312 "this_instr"
  , C.Seq 1312 1313
  , C.Assign 1313 "opcode" (Num 0)
  , C.Seq 1313 1314
  , C.Var 1314 "callable"
  , C.Seq 1314 1315
  , C.Var 1315 "self_or_null"
  , C.Seq 1315 1316
  , C.Var 1316 "args"
  , C.Seq 1316 1317
  , C.Var 1317 "res"
  , C.Seq 1317 1318
  , C.Assign 1318 "self_or_null" (Num 0)
  , C.Seq 1318 1319
  , C.Assign 1319 "callable" (Num 0)
  , C.Seq 1319 1320
  , C.Var 1320 "counter"
  , C.Seq 1320 1321
  , C.Branch 1321 (Eq (Num 0) (Num 1)) 1323 1326
  , C.Assign 1323 "next_instr" (Num 0)
  , C.Seq 1323 1324
  , C.Assign 1324 "stack_pointer" (Num 0)
  , C.Seq 1324 1325
  , C.Assign 1325 "opcode" (Num 0)
  , C.Seq 1325 1326
  , C.Seq 1325 35
  , C.Seq 1325 1327
  , C.Var 1326 "NOP_1326"
  , C.Seq 1326 1327
  , C.Var 1327 "IF_ELSE_FOOTER"
  , C.Assign 1328 "undefed" (Num 0)
  , C.Seq 1328 1329
  , C.Branch 1329 (Eq (Num 0) (Num 1)) 1330 1331
  , C.Assign 1330 "undefed" (Num 0)
  , C.Seq 1330 1329
  , C.Var 1331 "LOOP_FOOTER"
  , C.Seq 1331 1332
  , C.Branch 1332 (Eq (Plus (Plus (Num 0) (Plus (Id "PyMethod_Type") (Num 0))) (Plus (Num 0) (Num 0))) (Num 1)) 1334 1343
  , C.Var 1334 "callable_o"
  , C.Seq 1334 1335
  , C.Var 1335 "self"
  , C.Seq 1335 1336
  , C.Assign 1336 "self_or_null" (Num 0)
  , C.Seq 1336 1337
  , C.Var 1337 "method"
  , C.Seq 1337 1338
  , C.Var 1338 "temp"
  , C.Seq 1338 1339
  , C.Assign 1339 "callable" (Num 0)
  , C.Seq 1339 1340
  , C.Assign 1340 "undefed" (Num 0)
  , C.Seq 1340 1341
  , C.Assign 1341 "undefed" (Num 0)
  , C.Seq 1341 1342
  , C.Assign 1342 "stack_pointer" (Num 0)
  , C.Seq 1342 1343
  , C.Seq 1342 1344
  , C.Var 1343 "NOP_1343"
  , C.Seq 1343 1344
  , C.Var 1344 "IF_ELSE_FOOTER"
  , C.Assign 1345 "args" (Num 0)
  , C.Seq 1345 1346
  , C.Var 1346 "callable_o"
  , C.Seq 1346 1347
  , C.Var 1347 "total_args"
  , C.Seq 1347 1348
  , C.Var 1348 "arguments"
  , C.Seq 1348 1349
  , C.Branch 1349 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1351 1352
  , C.Assign 1351 "total_args" (Num 0)
  , C.Seq 1351 1352
  , C.Seq 1351 1353
  , C.Var 1352 "NOP_1352"
  , C.Seq 1352 1353
  , C.Var 1353 "IF_ELSE_FOOTER"
  , C.Branch 1354 (Eq (Plus (Plus (Plus (Num 0) (Plus (Id "PyFunction_Type") (Num 0))) (Plus (Num 0) (Num 0))) (Plus (Num 0) (Id "_PyFunction_Vectorcall"))) (Num 1)) 1356 1372
  , C.Var 1356 "code_flags"
  , C.Seq 1356 1357
  , C.Var 1357 "locals"
  , C.Seq 1357 1358
  , C.Assign 1358 "undefed" (Num 0)
  , C.Seq 1358 1359
  , C.Assign 1359 "undefed" (Num 0)
  , C.Seq 1359 1360
  , C.Var 1360 "new_frame"
  , C.Seq 1360 1361
  , C.Assign 1361 "stack_pointer" (Num 0)
  , C.Seq 1361 1362
  , C.Assign 1362 "stack_pointer" (Num 0)
  , C.Seq 1362 1363
  , C.Branch 1363 (Eq (Plus (Id "new_frame") (Num 0)) (Num 1)) 1365 1365
  , C.Seq 1364 1817
  , C.Seq 1364 1366
  , C.Var 1365 "NOP_1365"
  , C.Seq 1365 1366
  , C.Var 1366 "IF_ELSE_FOOTER"
  , C.Assign 1367 "undefed" (Num 0)
  , C.Seq 1367 1368
  , C.Assign 1368 "frame" (Num 0)
  , C.Seq 1368 1369
  , C.Seq 1368 1886
  , C.Branch 1369 (Eq (Num 0) (Num 1)) 1370 1371
  , C.Assign 1370 "frame" (Num 0)
  , C.Seq 1370 1371
  , C.Seq 1370 1886
  , C.Seq 1370 1369
  , C.Var 1371 "LOOP_FOOTER"
  , C.Seq 1371 1372
  , C.Seq 1371 1373
  , C.Var 1372 "NOP_1372"
  , C.Seq 1372 1373
  , C.Var 1373 "IF_ELSE_FOOTER"
  , C.Var 1374 "args_o_temp"
  , C.Seq 1374 1375
  , C.Var 1375 "args_o"
  , C.Seq 1375 1376
  , C.Branch 1376 (Eq (Plus (Id "args_o") (Num 0)) (Num 1)) 1378 1394
  , C.Var 1378 "tmp"
  , C.Seq 1378 1379
  , C.Var 1379 "_i"
  , C.Seq 1379 1380
  , C.Branch 1380 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1381 1385
  , C.Assign 1381 "tmp" (Num 0)
  , C.Seq 1381 1382
  , C.Assign 1382 "undefed" (Num 0)
  , C.Seq 1382 1383
  , C.Assign 1383 "undefed" (Num 0)
  , C.Seq 1383 1384
  , C.Assign 1384 "undefed" (Num 0)
  , C.Seq 1384 1385
  , C.Seq 1384 1380
  , C.Var 1385 "LOOP_FOOTER"
  , C.Seq 1385 1386
  , C.Assign 1386 "tmp" (Num 0)
  , C.Seq 1386 1387
  , C.Assign 1387 "self_or_null" (Num 0)
  , C.Seq 1387 1388
  , C.Assign 1388 "undefed" (Num 0)
  , C.Seq 1388 1389
  , C.Assign 1389 "tmp" (Num 0)
  , C.Seq 1389 1390
  , C.Assign 1390 "callable" (Num 0)
  , C.Seq 1390 1391
  , C.Assign 1391 "undefed" (Num 0)
  , C.Seq 1391 1392
  , C.Assign 1392 "stack_pointer" (Num 0)
  , C.Seq 1392 1393
  , C.Assign 1393 "stack_pointer" (Num 0)
  , C.Seq 1393 1394
  , C.Seq 1393 1817
  , C.Seq 1393 1395
  , C.Var 1394 "NOP_1394"
  , C.Seq 1394 1395
  , C.Var 1395 "IF_ELSE_FOOTER"
  , C.Assign 1396 "undefed" (Num 0)
  , C.Seq 1396 1397
  , C.Assign 1397 "undefed" (Num 0)
  , C.Seq 1397 1398
  , C.Var 1398 "res_o"
  , C.Seq 1398 1399
  , C.Assign 1399 "stack_pointer" (Num 0)
  , C.Seq 1399 1400
  , C.Branch 1400 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1402 1514
  , C.Var 1402 "arg"
  , C.Seq 1402 1403
  , C.Branch 1403 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 1405 1406
  , C.Assign 1405 "stack_pointer" (Num 0)
  , C.Seq 1405 1513
  , C.Var 1406 "err"
  , C.Seq 1406 1407
  , C.Assign 1407 "stack_pointer" (Num 0)
  , C.Seq 1407 1408
  , C.Branch 1408 (Eq (Plus (Id "err") (Num 0)) (Num 1)) 1410 1511
  , C.Assign 1410 "undefed" (Num 0)
  , C.Seq 1410 1411
  , C.Assign 1411 "_tmp_old_op" (Num 0)
  , C.Seq 1411 1412
  , C.Branch 1412 (Eq (Plus (Id "_tmp_old_op") (Num 0)) (Num 1)) 1414 1457
  , C.Assign 1414 "undefed" (Num 0)
  , C.Seq 1414 1415
  , C.Var 1415 "op"
  , C.Seq 1415 1416
  , C.Branch 1416 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1418 1433
  , C.Var 1418 "tracer"
  , C.Seq 1418 1419
  , C.Branch 1419 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1421 1422
  , C.Var 1421 "data"
  , C.Seq 1421 1422
  , C.Seq 1421 1423
  , C.Var 1422 "NOP_1422"
  , C.Seq 1422 1423
  , C.Var 1423 "IF_ELSE_FOOTER"
  , C.Branch 1424 (Eq (Num 0) (Num 1)) 1425 1431
  , C.Var 1425 "tracer"
  , C.Seq 1425 1426
  , C.Branch 1426 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1428 1429
  , C.Var 1428 "data"
  , C.Seq 1428 1429
  , C.Seq 1428 1430
  , C.Var 1429 "NOP_1429"
  , C.Seq 1429 1430
  , C.Var 1430 "IF_ELSE_FOOTER"
  , C.Seq 1430 1424
  , C.Var 1431 "LOOP_FOOTER"
  , C.Seq 1431 1432
  , C.Var 1432 "dealloc"
  , C.Seq 1432 1433
  , C.Seq 1432 1434
  , C.Var 1433 "NOP_1433"
  , C.Seq 1433 1434
  , C.Var 1434 "IF_ELSE_FOOTER"
  , C.Branch 1435 (Eq (Num 0) (Num 1)) 1436 1456
  , C.Var 1436 "op"
  , C.Seq 1436 1437
  , C.Branch 1437 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1439 1454
  , C.Var 1439 "tracer"
  , C.Seq 1439 1440
  , C.Branch 1440 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1442 1443
  , C.Var 1442 "data"
  , C.Seq 1442 1443
  , C.Seq 1442 1444
  , C.Var 1443 "NOP_1443"
  , C.Seq 1443 1444
  , C.Var 1444 "IF_ELSE_FOOTER"
  , C.Branch 1445 (Eq (Num 0) (Num 1)) 1446 1452
  , C.Var 1446 "tracer"
  , C.Seq 1446 1447
  , C.Branch 1447 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1449 1450
  , C.Var 1449 "data"
  , C.Seq 1449 1450
  , C.Seq 1449 1451
  , C.Var 1450 "NOP_1450"
  , C.Seq 1450 1451
  , C.Var 1451 "IF_ELSE_FOOTER"
  , C.Seq 1451 1445
  , C.Var 1452 "LOOP_FOOTER"
  , C.Seq 1452 1453
  , C.Var 1453 "dealloc"
  , C.Seq 1453 1454
  , C.Seq 1453 1455
  , C.Var 1454 "NOP_1454"
  , C.Seq 1454 1455
  , C.Var 1455 "IF_ELSE_FOOTER"
  , C.Seq 1455 1435
  , C.Var 1456 "LOOP_FOOTER"
  , C.Seq 1456 1457
  , C.Seq 1456 1458
  , C.Var 1457 "NOP_1457"
  , C.Seq 1457 1458
  , C.Var 1458 "IF_ELSE_FOOTER"
  , C.Branch 1459 (Eq (Num 0) (Num 1)) 1460 1509
  , C.Assign 1460 "undefed" (Num 0)
  , C.Seq 1460 1461
  , C.Assign 1461 "_tmp_old_op" (Num 0)
  , C.Seq 1461 1462
  , C.Branch 1462 (Eq (Plus (Id "_tmp_old_op") (Num 0)) (Num 1)) 1464 1507
  , C.Assign 1464 "undefed" (Num 0)
  , C.Seq 1464 1465
  , C.Var 1465 "op"
  , C.Seq 1465 1466
  , C.Branch 1466 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1468 1483
  , C.Var 1468 "tracer"
  , C.Seq 1468 1469
  , C.Branch 1469 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1471 1472
  , C.Var 1471 "data"
  , C.Seq 1471 1472
  , C.Seq 1471 1473
  , C.Var 1472 "NOP_1472"
  , C.Seq 1472 1473
  , C.Var 1473 "IF_ELSE_FOOTER"
  , C.Branch 1474 (Eq (Num 0) (Num 1)) 1475 1481
  , C.Var 1475 "tracer"
  , C.Seq 1475 1476
  , C.Branch 1476 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1478 1479
  , C.Var 1478 "data"
  , C.Seq 1478 1479
  , C.Seq 1478 1480
  , C.Var 1479 "NOP_1479"
  , C.Seq 1479 1480
  , C.Var 1480 "IF_ELSE_FOOTER"
  , C.Seq 1480 1474
  , C.Var 1481 "LOOP_FOOTER"
  , C.Seq 1481 1482
  , C.Var 1482 "dealloc"
  , C.Seq 1482 1483
  , C.Seq 1482 1484
  , C.Var 1483 "NOP_1483"
  , C.Seq 1483 1484
  , C.Var 1484 "IF_ELSE_FOOTER"
  , C.Branch 1485 (Eq (Num 0) (Num 1)) 1486 1506
  , C.Var 1486 "op"
  , C.Seq 1486 1487
  , C.Branch 1487 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1489 1504
  , C.Var 1489 "tracer"
  , C.Seq 1489 1490
  , C.Branch 1490 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1492 1493
  , C.Var 1492 "data"
  , C.Seq 1492 1493
  , C.Seq 1492 1494
  , C.Var 1493 "NOP_1493"
  , C.Seq 1493 1494
  , C.Var 1494 "IF_ELSE_FOOTER"
  , C.Branch 1495 (Eq (Num 0) (Num 1)) 1496 1502
  , C.Var 1496 "tracer"
  , C.Seq 1496 1497
  , C.Branch 1497 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1499 1500
  , C.Var 1499 "data"
  , C.Seq 1499 1500
  , C.Seq 1499 1501
  , C.Var 1500 "NOP_1500"
  , C.Seq 1500 1501
  , C.Var 1501 "IF_ELSE_FOOTER"
  , C.Seq 1501 1495
  , C.Var 1502 "LOOP_FOOTER"
  , C.Seq 1502 1503
  , C.Var 1503 "dealloc"
  , C.Seq 1503 1504
  , C.Seq 1503 1505
  , C.Var 1504 "NOP_1504"
  , C.Seq 1504 1505
  , C.Var 1505 "IF_ELSE_FOOTER"
  , C.Seq 1505 1485
  , C.Var 1506 "LOOP_FOOTER"
  , C.Seq 1506 1507
  , C.Seq 1506 1508
  , C.Var 1507 "NOP_1507"
  , C.Seq 1507 1508
  , C.Var 1508 "IF_ELSE_FOOTER"
  , C.Seq 1508 1459
  , C.Var 1509 "LOOP_FOOTER"
  , C.Seq 1509 1510
  , C.Assign 1510 "stack_pointer" (Num 0)
  , C.Seq 1510 1511
  , C.Seq 1510 1512
  , C.Var 1511 "NOP_1511"
  , C.Seq 1511 1512
  , C.Var 1512 "IF_ELSE_FOOTER"
  , C.Var 1513 "IF_ELSE_FOOTER"
  , C.Seq 1513 1515
  , C.Var 1514 "NOP_1514"
  , C.Seq 1514 1515
  , C.Var 1515 "IF_ELSE_FOOTER"
  , C.Var 1516 "tmp"
  , C.Seq 1516 1517
  , C.Var 1517 "_i"
  , C.Seq 1517 1518
  , C.Branch 1518 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1519 1521
  , C.Assign 1519 "tmp" (Num 0)
  , C.Seq 1519 1520
  , C.Assign 1520 "undefed" (Num 0)
  , C.Seq 1520 1521
  , C.Seq 1520 1518
  , C.Var 1521 "LOOP_FOOTER"
  , C.Seq 1521 1522
  , C.Assign 1522 "tmp" (Num 0)
  , C.Seq 1522 1523
  , C.Assign 1523 "self_or_null" (Num 0)
  , C.Seq 1523 1524
  , C.Assign 1524 "undefed" (Num 0)
  , C.Seq 1524 1525
  , C.Assign 1525 "tmp" (Num 0)
  , C.Seq 1525 1526
  , C.Assign 1526 "callable" (Num 0)
  , C.Seq 1526 1527
  , C.Assign 1527 "undefed" (Num 0)
  , C.Seq 1527 1528
  , C.Assign 1528 "stack_pointer" (Num 0)
  , C.Seq 1528 1529
  , C.Assign 1529 "stack_pointer" (Num 0)
  , C.Seq 1529 1530
  , C.Branch 1530 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 1532 1532
  , C.Seq 1531 1817
  , C.Seq 1531 1533
  , C.Var 1532 "NOP_1532"
  , C.Seq 1532 1533
  , C.Var 1533 "IF_ELSE_FOOTER"
  , C.Assign 1534 "res" (Num 0)
  , C.Seq 1534 1535
  , C.Assign 1535 "undefed" (Num 0)
  , C.Seq 1535 1536
  , C.Assign 1536 "stack_pointer" (Num 0)
  , C.Seq 1536 1537
  , C.Var 1537 "err"
  , C.Seq 1537 1538
  , C.Assign 1538 "stack_pointer" (Num 0)
  , C.Seq 1538 1539
  , C.Branch 1539 (Eq (Plus (Id "err") (Num 0)) (Num 1)) 1541 1541
  , C.Seq 1540 1817
  , C.Seq 1540 1542
  , C.Var 1541 "NOP_1541"
  , C.Seq 1541 1542
  , C.Var 1542 "IF_ELSE_FOOTER"
  , C.Var 1543 "word"
  , C.Seq 1543 1544
  , C.Assign 1544 "opcode" (Num 0)
  , C.Seq 1544 1545
  , C.Assign 1545 "oparg" (Num 0)
  , C.Seq 1545 1546
  , C.Branch 1546 (Eq (Num 0) (Num 1)) 1547 1550
  , C.Var 1547 "word"
  , C.Seq 1547 1548
  , C.Assign 1548 "opcode" (Num 0)
  , C.Seq 1548 1549
  , C.Assign 1549 "oparg" (Num 0)
  , C.Seq 1549 1550
  , C.Seq 1549 1546
  , C.Var 1550 "LOOP_FOOTER"
  , C.Seq 1550 1551
  , C.Seq 1550 35
  , C.Branch 1551 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1553 1637
  , C.Var 1553 "NOP_1553"
  , C.Var 1554 "__CLABEL_TARGET_CALL_ALLOC_AND_ENTER_INIT"
  , C.Seq 1554 1555
  , C.Var 1555 "this_instr"
  , C.Seq 1555 1556
  , C.Assign 1556 "undefed" (Num 0)
  , C.Seq 1556 1557
  , C.Assign 1557 "next_instr" (Num 0)
  , C.Seq 1557 1558
  , C.Var 1558 "callable"
  , C.Seq 1558 1559
  , C.Var 1559 "self_or_null"
  , C.Seq 1559 1560
  , C.Var 1560 "init"
  , C.Seq 1560 1561
  , C.Var 1561 "self"
  , C.Seq 1561 1562
  , C.Var 1562 "args"
  , C.Seq 1562 1563
  , C.Var 1563 "init_frame"
  , C.Seq 1563 1564
  , C.Var 1564 "new_frame"
  , C.Seq 1564 1565
  , C.Branch 1565 (Eq (Num 0) (Num 1)) 1567 1567
  , C.Seq 1566 1310
  , C.Seq 1566 1568
  , C.Var 1567 "NOP_1567"
  , C.Seq 1567 1568
  , C.Var 1568 "IF_ELSE_FOOTER"
  , C.Assign 1569 "self_or_null" (Num 0)
  , C.Seq 1569 1570
  , C.Assign 1570 "callable" (Num 0)
  , C.Seq 1570 1571
  , C.Var 1571 "type_version"
  , C.Seq 1571 1572
  , C.Var 1572 "callable_o"
  , C.Seq 1572 1573
  , C.Branch 1573 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1575 1575
  , C.Seq 1574 1310
  , C.Seq 1574 1576
  , C.Var 1575 "NOP_1575"
  , C.Seq 1575 1576
  , C.Var 1576 "IF_ELSE_FOOTER"
  , C.Branch 1577 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1579 1579
  , C.Seq 1578 1310
  , C.Seq 1578 1580
  , C.Var 1579 "NOP_1579"
  , C.Seq 1579 1580
  , C.Var 1580 "IF_ELSE_FOOTER"
  , C.Var 1581 "tp"
  , C.Seq 1581 1582
  , C.Branch 1582 (Eq (Plus (Num 0) (Id "type_version")) (Num 1)) 1584 1584
  , C.Seq 1583 1310
  , C.Seq 1583 1585
  , C.Var 1584 "NOP_1584"
  , C.Seq 1584 1585
  , C.Var 1585 "IF_ELSE_FOOTER"
  , C.Var 1586 "cls"
  , C.Seq 1586 1587
  , C.Var 1587 "init_func"
  , C.Seq 1587 1588
  , C.Var 1588 "code"
  , C.Seq 1588 1589
  , C.Branch 1589 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1591 1591
  , C.Seq 1590 1310
  , C.Seq 1590 1592
  , C.Var 1591 "NOP_1591"
  , C.Seq 1591 1592
  , C.Var 1592 "IF_ELSE_FOOTER"
  , C.Var 1593 "self_o"
  , C.Seq 1593 1594
  , C.Assign 1594 "stack_pointer" (Num 0)
  , C.Seq 1594 1595
  , C.Branch 1595 (Eq (Plus (Id "self_o") (Num 0)) (Num 1)) 1597 1597
  , C.Seq 1596 1817
  , C.Seq 1596 1598
  , C.Var 1597 "NOP_1597"
  , C.Seq 1597 1598
  , C.Var 1598 "IF_ELSE_FOOTER"
  , C.Assign 1599 "self_or_null" (Num 0)
  , C.Seq 1599 1600
  , C.Var 1600 "temp"
  , C.Seq 1600 1601
  , C.Assign 1601 "callable" (Num 0)
  , C.Seq 1601 1602
  , C.Assign 1602 "undefed" (Num 0)
  , C.Seq 1602 1603
  , C.Assign 1603 "undefed" (Num 0)
  , C.Seq 1603 1604
  , C.Assign 1604 "stack_pointer" (Num 0)
  , C.Seq 1604 1605
  , C.Assign 1605 "args" (Num 0)
  , C.Seq 1605 1606
  , C.Assign 1606 "self" (Num 0)
  , C.Seq 1606 1607
  , C.Assign 1607 "init" (Num 0)
  , C.Seq 1607 1608
  , C.Var 1608 "shim"
  , C.Seq 1608 1609
  , C.Assign 1609 "stack_pointer" (Num 0)
  , C.Seq 1609 1610
  , C.Assign 1610 "undefed" (Num 0)
  , C.Seq 1610 1611
  , C.Var 1611 "temp"
  , C.Seq 1611 1612
  , C.Assign 1612 "stack_pointer" (Num 0)
  , C.Seq 1612 1613
  , C.Assign 1613 "stack_pointer" (Num 0)
  , C.Seq 1613 1614
  , C.Branch 1614 (Eq (Plus (Id "temp") (Num 0)) (Num 1)) 1616 1617
  , C.Assign 1616 "stack_pointer" (Num 0)
  , C.Seq 1616 1617
  , C.Seq 1616 1817
  , C.Seq 1616 1618
  , C.Var 1617 "NOP_1617"
  , C.Seq 1617 1618
  , C.Var 1618 "IF_ELSE_FOOTER"
  , C.Assign 1619 "undefed" (Num 0)
  , C.Seq 1619 1620
  , C.Assign 1620 "init_frame" (Num 0)
  , C.Seq 1620 1621
  , C.Assign 1621 "new_frame" (Num 0)
  , C.Seq 1621 1622
  , C.Var 1622 "temp"
  , C.Seq 1622 1623
  , C.Assign 1623 "frame" (Num 0)
  , C.Seq 1623 1624
  , C.Assign 1624 "stack_pointer" (Num 0)
  , C.Seq 1624 1625
  , C.Assign 1625 "next_instr" (Num 0)
  , C.Seq 1625 1626
  , C.Branch 1626 (Eq (Num 0) (Num 1)) 1627 1628
  , C.Assign 1627 "next_instr" (Num 0)
  , C.Seq 1627 1626
  , C.Var 1628 "LOOP_FOOTER"
  , C.Seq 1628 1629
  , C.Var 1629 "word"
  , C.Seq 1629 1630
  , C.Assign 1630 "opcode" (Num 0)
  , C.Seq 1630 1631
  , C.Assign 1631 "oparg" (Num 0)
  , C.Seq 1631 1632
  , C.Branch 1632 (Eq (Num 0) (Num 1)) 1633 1636
  , C.Var 1633 "word"
  , C.Seq 1633 1634
  , C.Assign 1634 "opcode" (Num 0)
  , C.Seq 1634 1635
  , C.Assign 1635 "oparg" (Num 0)
  , C.Seq 1635 1636
  , C.Seq 1635 1632
  , C.Var 1636 "LOOP_FOOTER"
  , C.Seq 1636 1637
  , C.Seq 1636 35
  , C.Branch 1637 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1639 1727
  , C.Var 1639 "NOP_1639"
  , C.Var 1640 "__CLABEL_TARGET_CALL_BOUND_METHOD_EXACT_ARGS"
  , C.Seq 1640 1641
  , C.Var 1641 "this_instr"
  , C.Seq 1641 1642
  , C.Assign 1642 "undefed" (Num 0)
  , C.Seq 1642 1643
  , C.Assign 1643 "next_instr" (Num 0)
  , C.Seq 1643 1644
  , C.Var 1644 "callable"
  , C.Seq 1644 1645
  , C.Var 1645 "null"
  , C.Seq 1645 1646
  , C.Var 1646 "self_or_null"
  , C.Seq 1646 1647
  , C.Var 1647 "args"
  , C.Seq 1647 1648
  , C.Var 1648 "new_frame"
  , C.Seq 1648 1649
  , C.Branch 1649 (Eq (Num 0) (Num 1)) 1651 1651
  , C.Seq 1650 1310
  , C.Seq 1650 1652
  , C.Var 1651 "NOP_1651"
  , C.Seq 1651 1652
  , C.Var 1652 "IF_ELSE_FOOTER"
  , C.Assign 1653 "null" (Num 0)
  , C.Seq 1653 1654
  , C.Assign 1654 "callable" (Num 0)
  , C.Seq 1654 1655
  , C.Branch 1655 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1657 1657
  , C.Seq 1656 1310
  , C.Seq 1656 1658
  , C.Var 1657 "NOP_1657"
  , C.Seq 1657 1658
  , C.Var 1658 "IF_ELSE_FOOTER"
  , C.Branch 1659 (Eq (Plus (Num 0) (Plus (Id "PyMethod_Type") (Num 0))) (Num 1)) 1661 1661
  , C.Seq 1660 1310
  , C.Seq 1660 1662
  , C.Var 1661 "NOP_1661"
  , C.Seq 1661 1662
  , C.Var 1662 "IF_ELSE_FOOTER"
  , C.Assign 1663 "self_or_null" (Num 0)
  , C.Seq 1663 1664
  , C.Var 1664 "callable_o"
  , C.Seq 1664 1665
  , C.Assign 1665 "self_or_null" (Num 0)
  , C.Seq 1665 1666
  , C.Var 1666 "temp"
  , C.Seq 1666 1667
  , C.Assign 1667 "callable" (Num 0)
  , C.Seq 1667 1668
  , C.Assign 1668 "undefed" (Num 0)
  , C.Seq 1668 1669
  , C.Assign 1669 "undefed" (Num 0)
  , C.Seq 1669 1670
  , C.Assign 1670 "stack_pointer" (Num 0)
  , C.Seq 1670 1671
  , C.Var 1671 "func_version"
  , C.Seq 1671 1672
  , C.Var 1672 "callable_o"
  , C.Seq 1672 1673
  , C.Branch 1673 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFunction_Type") (Num 0))) (Num 0)) (Num 1)) 1675 1675
  , C.Seq 1674 1310
  , C.Seq 1674 1676
  , C.Var 1675 "NOP_1675"
  , C.Seq 1675 1676
  , C.Var 1676 "IF_ELSE_FOOTER"
  , C.Var 1677 "func"
  , C.Seq 1677 1678
  , C.Branch 1678 (Eq (Plus (Num 0) (Id "func_version")) (Num 1)) 1680 1680
  , C.Seq 1679 1310
  , C.Seq 1679 1681
  , C.Var 1680 "NOP_1680"
  , C.Seq 1680 1681
  , C.Var 1681 "IF_ELSE_FOOTER"
  , C.Var 1682 "callable_o"
  , C.Seq 1682 1683
  , C.Var 1683 "func"
  , C.Seq 1683 1684
  , C.Var 1684 "code"
  , C.Seq 1684 1685
  , C.Branch 1685 (Eq (Plus (Num 0) (Plus (Id "oparg") (Plus (Plus (Num 0) (Num 0)) (Num 0)))) (Num 1)) 1687 1687
  , C.Seq 1686 1310
  , C.Seq 1686 1688
  , C.Var 1687 "NOP_1687"
  , C.Seq 1687 1688
  , C.Var 1688 "IF_ELSE_FOOTER"
  , C.Var 1689 "callable_o"
  , C.Seq 1689 1690
  , C.Var 1690 "func"
  , C.Seq 1690 1691
  , C.Var 1691 "code"
  , C.Seq 1691 1692
  , C.Branch 1692 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1694 1694
  , C.Seq 1693 1310
  , C.Seq 1693 1695
  , C.Var 1694 "NOP_1694"
  , C.Seq 1694 1695
  , C.Var 1695 "IF_ELSE_FOOTER"
  , C.Branch 1696 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1698 1698
  , C.Seq 1697 1310
  , C.Seq 1697 1699
  , C.Var 1698 "NOP_1698"
  , C.Seq 1698 1699
  , C.Var 1699 "IF_ELSE_FOOTER"
  , C.Assign 1700 "args" (Num 0)
  , C.Seq 1700 1701
  , C.Var 1701 "has_self"
  , C.Seq 1701 1702
  , C.Var 1702 "pushed_frame"
  , C.Seq 1702 1703
  , C.Var 1703 "first_non_self_local"
  , C.Seq 1703 1704
  , C.Assign 1704 "undefed" (Num 0)
  , C.Seq 1704 1705
  , C.Var 1705 "i"
  , C.Seq 1705 1706
  , C.Branch 1706 (Eq (Plus (Id "i") (Id "oparg")) (Num 1)) 1707 1708
  , C.Assign 1707 "undefed" (Num 0)
  , C.Seq 1707 1708
  , C.Seq 1707 1706
  , C.Var 1708 "LOOP_FOOTER"
  , C.Seq 1708 1709
  , C.Assign 1709 "new_frame" (Num 0)
  , C.Seq 1709 1710
  , C.Assign 1710 "undefed" (Num 0)
  , C.Seq 1710 1711
  , C.Var 1711 "temp"
  , C.Seq 1711 1712
  , C.Assign 1712 "stack_pointer" (Num 0)
  , C.Seq 1712 1713
  , C.Assign 1713 "frame" (Num 0)
  , C.Seq 1713 1714
  , C.Assign 1714 "stack_pointer" (Num 0)
  , C.Seq 1714 1715
  , C.Assign 1715 "next_instr" (Num 0)
  , C.Seq 1715 1716
  , C.Branch 1716 (Eq (Num 0) (Num 1)) 1717 1718
  , C.Assign 1717 "next_instr" (Num 0)
  , C.Seq 1717 1716
  , C.Var 1718 "LOOP_FOOTER"
  , C.Seq 1718 1719
  , C.Var 1719 "word"
  , C.Seq 1719 1720
  , C.Assign 1720 "opcode" (Num 0)
  , C.Seq 1720 1721
  , C.Assign 1721 "oparg" (Num 0)
  , C.Seq 1721 1722
  , C.Branch 1722 (Eq (Num 0) (Num 1)) 1723 1726
  , C.Var 1723 "word"
  , C.Seq 1723 1724
  , C.Assign 1724 "opcode" (Num 0)
  , C.Seq 1724 1725
  , C.Assign 1725 "oparg" (Num 0)
  , C.Seq 1725 1726
  , C.Seq 1725 1722
  , C.Var 1726 "LOOP_FOOTER"
  , C.Seq 1726 1727
  , C.Seq 1726 35
  , C.Branch 1727 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1729 1810
  , C.Var 1729 "NOP_1729"
  , C.Var 1730 "__CLABEL_TARGET_CALL_BOUND_METHOD_GENERAL"
  , C.Seq 1730 1731
  , C.Var 1731 "this_instr"
  , C.Seq 1731 1732
  , C.Assign 1732 "undefed" (Num 0)
  , C.Seq 1732 1733
  , C.Assign 1733 "next_instr" (Num 0)
  , C.Seq 1733 1734
  , C.Var 1734 "callable"
  , C.Seq 1734 1735
  , C.Var 1735 "null"
  , C.Seq 1735 1736
  , C.Var 1736 "self_or_null"
  , C.Seq 1736 1737
  , C.Var 1737 "args"
  , C.Seq 1737 1738
  , C.Var 1738 "new_frame"
  , C.Seq 1738 1739
  , C.Branch 1739 (Eq (Num 0) (Num 1)) 1741 1741
  , C.Seq 1740 1310
  , C.Seq 1740 1742
  , C.Var 1741 "NOP_1741"
  , C.Seq 1741 1742
  , C.Var 1742 "IF_ELSE_FOOTER"
  , C.Assign 1743 "null" (Num 0)
  , C.Seq 1743 1744
  , C.Assign 1744 "callable" (Num 0)
  , C.Seq 1744 1745
  , C.Var 1745 "func_version"
  , C.Seq 1745 1746
  , C.Var 1746 "callable_o"
  , C.Seq 1746 1747
  , C.Branch 1747 (Eq (Plus (Num 0) (Plus (Id "PyMethod_Type") (Num 0))) (Num 1)) 1749 1749
  , C.Seq 1748 1310
  , C.Seq 1748 1750
  , C.Var 1749 "NOP_1749"
  , C.Seq 1749 1750
  , C.Var 1750 "IF_ELSE_FOOTER"
  , C.Var 1751 "func"
  , C.Seq 1751 1752
  , C.Branch 1752 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFunction_Type") (Num 0))) (Num 0)) (Num 1)) 1754 1754
  , C.Seq 1753 1310
  , C.Seq 1753 1755
  , C.Var 1754 "NOP_1754"
  , C.Seq 1754 1755
  , C.Var 1755 "IF_ELSE_FOOTER"
  , C.Branch 1756 (Eq (Plus (Num 0) (Id "func_version")) (Num 1)) 1758 1758
  , C.Seq 1757 1310
  , C.Seq 1757 1759
  , C.Var 1758 "NOP_1758"
  , C.Seq 1758 1759
  , C.Var 1759 "IF_ELSE_FOOTER"
  , C.Branch 1760 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1762 1762
  , C.Seq 1761 1310
  , C.Seq 1761 1763
  , C.Var 1762 "NOP_1762"
  , C.Seq 1762 1763
  , C.Var 1763 "IF_ELSE_FOOTER"
  , C.Assign 1764 "self_or_null" (Num 0)
  , C.Seq 1764 1765
  , C.Var 1765 "callable_o"
  , C.Seq 1765 1766
  , C.Assign 1766 "self_or_null" (Num 0)
  , C.Seq 1766 1767
  , C.Var 1767 "temp"
  , C.Seq 1767 1768
  , C.Assign 1768 "callable" (Num 0)
  , C.Seq 1768 1769
  , C.Assign 1769 "undefed" (Num 0)
  , C.Seq 1769 1770
  , C.Assign 1770 "undefed" (Num 0)
  , C.Seq 1770 1771
  , C.Assign 1771 "stack_pointer" (Num 0)
  , C.Seq 1771 1772
  , C.Branch 1772 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1774 1774
  , C.Seq 1773 1310
  , C.Seq 1773 1775
  , C.Var 1774 "NOP_1774"
  , C.Seq 1774 1775
  , C.Var 1775 "IF_ELSE_FOOTER"
  , C.Assign 1776 "args" (Num 0)
  , C.Seq 1776 1777
  , C.Var 1777 "callable_o"
  , C.Seq 1777 1778
  , C.Var 1778 "total_args"
  , C.Seq 1778 1779
  , C.Branch 1779 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1781 1782
  , C.Assign 1781 "total_args" (Num 0)
  , C.Seq 1781 1782
  , C.Seq 1781 1783
  , C.Var 1782 "NOP_1782"
  , C.Seq 1782 1783
  , C.Var 1783 "IF_ELSE_FOOTER"
  , C.Var 1784 "code_flags"
  , C.Seq 1784 1785
  , C.Var 1785 "locals"
  , C.Seq 1785 1786
  , C.Var 1786 "temp"
  , C.Seq 1786 1787
  , C.Assign 1787 "stack_pointer" (Num 0)
  , C.Seq 1787 1788
  , C.Assign 1788 "stack_pointer" (Num 0)
  , C.Seq 1788 1789
  , C.Branch 1789 (Eq (Plus (Id "temp") (Num 0)) (Num 1)) 1791 1791
  , C.Seq 1790 1817
  , C.Seq 1790 1792
  , C.Var 1791 "NOP_1791"
  , C.Seq 1791 1792
  , C.Var 1792 "IF_ELSE_FOOTER"
  , C.Assign 1793 "new_frame" (Num 0)
  , C.Seq 1793 1794
  , C.Assign 1794 "undefed" (Num 0)
  , C.Seq 1794 1795
  , C.Var 1795 "temp"
  , C.Seq 1795 1796
  , C.Assign 1796 "frame" (Num 0)
  , C.Seq 1796 1797
  , C.Assign 1797 "stack_pointer" (Num 0)
  , C.Seq 1797 1798
  , C.Assign 1798 "next_instr" (Num 0)
  , C.Seq 1798 1799
  , C.Branch 1799 (Eq (Num 0) (Num 1)) 1800 1801
  , C.Assign 1800 "next_instr" (Num 0)
  , C.Seq 1800 1799
  , C.Var 1801 "LOOP_FOOTER"
  , C.Seq 1801 1802
  , C.Var 1802 "word"
  , C.Seq 1802 1803
  , C.Assign 1803 "opcode" (Num 0)
  , C.Seq 1803 1804
  , C.Assign 1804 "oparg" (Num 0)
  , C.Seq 1804 1805
  , C.Branch 1805 (Eq (Num 0) (Num 1)) 1806 1809
  , C.Var 1806 "word"
  , C.Seq 1806 1807
  , C.Assign 1807 "opcode" (Num 0)
  , C.Seq 1807 1808
  , C.Assign 1808 "oparg" (Num 0)
  , C.Seq 1808 1809
  , C.Seq 1808 1805
  , C.Var 1809 "LOOP_FOOTER"
  , C.Seq 1809 1810
  , C.Seq 1809 35
  , C.Var 1810 "NOP_1810"
  , C.Seq 1810 1811
  , C.Var 1811 "__CLABEL_CODEGEN_SWITCH_EXIT_0"
  , C.Seq 1811 1812
  , C.Var 1812 "NOP_1812"
  , C.Var 1813 "__CLABEL_pop_2_error"
  , C.Seq 1813 1814
  , C.Assign 1814 "stack_pointer" (Num 0)
  , C.Seq 1814 1815
  , C.Seq 1814 1817
  , C.Var 1815 "__CLABEL_pop_1_error"
  , C.Seq 1815 1816
  , C.Assign 1816 "stack_pointer" (Num 0)
  , C.Seq 1816 1817
  , C.Seq 1816 1817
  , C.Var 1817 "__CLABEL_error"
  , C.Seq 1817 1818
  , C.Branch 1818 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1820 1821
  , C.Assign 1820 "stack_pointer" (Num 0)
  , C.Seq 1820 1822
  , C.Var 1821 "NOP_1821"
  , C.Seq 1821 1822
  , C.Var 1822 "IF_ELSE_FOOTER"
  , C.Branch 1823 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1825 1832
  , C.Var 1825 "f"
  , C.Seq 1825 1826
  , C.Assign 1826 "stack_pointer" (Num 0)
  , C.Seq 1826 1827
  , C.Branch 1827 (Eq (Plus (Id "f") (Num 0)) (Num 1)) 1829 1830
  , C.Assign 1829 "stack_pointer" (Num 0)
  , C.Seq 1829 1831
  , C.Var 1830 "NOP_1830"
  , C.Seq 1830 1831
  , C.Var 1831 "IF_ELSE_FOOTER"
  , C.Seq 1831 1833
  , C.Var 1832 "NOP_1832"
  , C.Seq 1832 1833
  , C.Var 1833 "IF_ELSE_FOOTER"
  , C.Seq 1833 1834
  , C.Var 1834 "__CLABEL_exception_unwind"
  , C.Seq 1834 1835
  , C.Var 1835 "offset"
  , C.Seq 1835 1836
  , C.Var 1836 "level"
  , C.Seq 1836 1837
  , C.Var 1837 "handler"
  , C.Seq 1837 1838
  , C.Var 1838 "lasti"
  , C.Seq 1838 1839
  , C.Var 1839 "handled"
  , C.Seq 1839 1840
  , C.Branch 1840 (Eq (Plus (Id "handled") (Num 0)) (Num 1)) 1842 1846
  , C.Var 1842 "stackbase"
  , C.Seq 1842 1843
  , C.Branch 1843 (Eq (Plus (Num 0) (Id "stackbase")) (Num 1)) 1844 1845
  , C.Var 1844 "ref"
  , C.Seq 1844 1845
  , C.Seq 1844 1843
  , C.Var 1845 "LOOP_FOOTER"
  , C.Seq 1845 1846
  , C.Seq 1845 1874
  , C.Seq 1845 1847
  , C.Var 1846 "NOP_1846"
  , C.Seq 1846 1847
  , C.Var 1847 "IF_ELSE_FOOTER"
  , C.Var 1848 "new_top"
  , C.Seq 1848 1849
  , C.Branch 1849 (Eq (Plus (Num 0) (Id "new_top")) (Num 1)) 1850 1851
  , C.Var 1850 "ref"
  , C.Seq 1850 1851
  , C.Seq 1850 1849
  , C.Var 1851 "LOOP_FOOTER"
  , C.Seq 1851 1852
  , C.Branch 1852 (Eq (Id "lasti") (Num 1)) 1854 1856
  , C.Var 1854 "frame_lasti"
  , C.Seq 1854 1855
  , C.Var 1855 "lasti"
  , C.Seq 1855 1856
  , C.Seq 1855 1857
  , C.Var 1856 "NOP_1856"
  , C.Seq 1856 1857
  , C.Var 1857 "IF_ELSE_FOOTER"
  , C.Var 1858 "exc"
  , C.Seq 1858 1859
  , C.Assign 1859 "next_instr" (Num 0)
  , C.Seq 1859 1860
  , C.Var 1860 "err"
  , C.Seq 1860 1861
  , C.Branch 1861 (Eq (Plus (Id "err") (Num 0)) (Num 1)) 1863 1863
  , C.Seq 1862 1834
  , C.Seq 1862 1864
  , C.Var 1863 "NOP_1863"
  , C.Seq 1863 1864
  , C.Var 1864 "IF_ELSE_FOOTER"
  , C.Assign 1865 "stack_pointer" (Num 0)
  , C.Seq 1865 1866
  , C.Var 1866 "word"
  , C.Seq 1866 1867
  , C.Assign 1867 "opcode" (Num 0)
  , C.Seq 1867 1868
  , C.Assign 1868 "oparg" (Num 0)
  , C.Seq 1868 1869
  , C.Branch 1869 (Eq (Num 0) (Num 1)) 1870 1873
  , C.Var 1870 "word"
  , C.Seq 1870 1871
  , C.Assign 1871 "opcode" (Num 0)
  , C.Seq 1871 1872
  , C.Assign 1872 "oparg" (Num 0)
  , C.Seq 1872 1873
  , C.Seq 1872 1869
  , C.Var 1873 "LOOP_FOOTER"
  , C.Seq 1873 1874
  , C.Seq 1873 35
  , C.Var 1874 "__CLABEL_exit_unwind"
  , C.Seq 1874 1875
  , C.Var 1875 "dying"
  , C.Seq 1875 1876
  , C.Assign 1876 "frame" (Num 0)
  , C.Seq 1876 1877
  , C.Assign 1877 "undefed" (Num 0)
  , C.Seq 1877 1878
  , C.Branch 1878 (Eq (Plus (Num 0) (Id "FRAME_OWNED_BY_INTERPRETER")) (Num 1)) 1880 1882
  , C.Assign 1880 "undefed" (Num 0)
  , C.Seq 1880 1881
  , C.Assign 1881 "return" (Num 0)
  , C.Seq 1881 1882
  , C.Seq 1881 1883
  , C.Var 1882 "NOP_1882"
  , C.Seq 1882 1883
  , C.Var 1883 "IF_ELSE_FOOTER"
  , C.Assign 1884 "next_instr" (Num 0)
  , C.Seq 1884 1885
  , C.Assign 1885 "stack_pointer" (Num 0)
  , C.Seq 1885 1886
  , C.Seq 1885 1817
  , C.Var 1886 "__CLABEL_start_frame"
  , C.Seq 1886 1887
  , C.Var 1887 "too_deep"
  , C.Seq 1887 1888
  , C.Branch 1888 (Eq (Id "too_deep") (Num 1)) 1890 1890
  , C.Seq 1889 1874
  , C.Seq 1889 1891
  , C.Var 1890 "NOP_1890"
  , C.Seq 1890 1891
  , C.Var 1891 "IF_ELSE_FOOTER"
  , C.Assign 1892 "next_instr" (Num 0)
  , C.Seq 1892 1893
  , C.Assign 1893 "stack_pointer" (Num 0)
  , C.Seq 1893 1894
  , C.Var 1894 "word"
  , C.Seq 1894 1895
  , C.Assign 1895 "opcode" (Num 0)
  , C.Seq 1895 1896
  , C.Assign 1896 "oparg" (Num 0)
  , C.Seq 1896 1897
  , C.Branch 1897 (Eq (Num 0) (Num 1)) 1898 1901
  , C.Var 1898 "word"
  , C.Seq 1898 1899
  , C.Assign 1899 "opcode" (Num 0)
  , C.Seq 1899 1900
  , C.Assign 1900 "oparg" (Num 0)
  , C.Seq 1900 1901
  , C.Seq 1900 1897
  , C.Var 1901 "LOOP_FOOTER"
  , C.Seq 1901 1902
  , C.Seq 1901 35
  , C.Var 1902 "__CLABEL_early_exit"
  , C.Seq 1902 1903
  , C.Var 1903 "NOP_1903"
  , C.Var 1904 "dying"
  , C.Seq 1904 1905
  , C.Assign 1905 "frame" (Num 0)
  , C.Seq 1905 1906
  , C.Assign 1906 "undefed" (Num 0)
  , C.Seq 1906 1907
  , C.Assign 1907 "undefed" (Num 0)
  , C.Seq 1907 1908
  , C.Assign 1908 "return" (Num 0)
  , C.Seq 1908 1909
  , C.Seq 1908 1909
  , C.Var 1909 "PROG_END"
  ]

customNTest :: [CN.Fact]
customNTest =
  [ CN.Var 0 "_PyEval_EvalFrameDefault"
  , CN.Seq 0 1
  , CN.Var 1 "tstate"
  , CN.Seq 1 2
  , CN.Var 2 "frame"
  , CN.Seq 2 3
  , CN.Var 3 "throwflag"
  , CN.Seq 3 4
  , CN.Var 4 "opcode"
  , CN.Seq 4 5
  , CN.Var 5 "oparg"
  , CN.Seq 5 6
  , CN.Var 6 "entry"
  , CN.Seq 6 7
  , CN.Branch 7 (Eq (Num 0) (Num 1)) 9 10
  , CN.Assign 9 "return" (Num 0)
  , CN.Seq 9 11
  , CN.Var 10 "NOP_10"
  , CN.Seq 10 11
  , CN.Var 11 "IF_ELSE_FOOTER"
  , CN.Var 12 "next_instr"
  , CN.Seq 12 13
  , CN.Var 13 "stack_pointer"
  , CN.Seq 13 14
  , CN.Assign 14 "undefed" (Num 0)
  , CN.Seq 14 15
  , CN.Assign 15 "undefed" (Num 0)
  , CN.Seq 15 16
  , CN.Assign 16 "undefed" (Num 0)
  , CN.Seq 16 17
  , CN.Assign 17 "undefed" (Num 0)
  , CN.Seq 17 18
  , CN.Assign 18 "undefed" (Num 0)
  , CN.Seq 18 19
  , CN.Assign 19 "undefed" (Num 0)
  , CN.Seq 19 20
  , CN.Assign 20 "undefed" (Num 0)
  , CN.Seq 20 21
  , CN.Assign 21 "undefed" (Num 0)
  , CN.Seq 21 22
  , CN.Assign 22 "undefed" (Num 0)
  , CN.Seq 22 23
  , CN.Assign 23 "undefed" (Num 0)
  , CN.Seq 23 24
  , CN.Assign 24 "undefed" (Num 0)
  , CN.Seq 24 25
  , CN.Branch 25 (Eq (Id "throwflag") (Num 1)) 27 33
  , CN.Branch 27 (Eq (Num 0) (Num 1)) 29 29
  , CN.Seq 28 1902
  , CN.Seq 28 30
  , CN.Var 29 "NOP_29"
  , CN.Seq 29 30
  , CN.Var 30 "IF_ELSE_FOOTER"
  , CN.Assign 31 "next_instr" (Num 0)
  , CN.Seq 31 32
  , CN.Assign 32 "stack_pointer" (Num 0)
  , CN.Seq 32 33
  , CN.Seq 32 1817
  , CN.Seq 32 34
  , CN.Var 33 "NOP_33"
  , CN.Seq 33 34
  , CN.Var 34 "IF_ELSE_FOOTER"
  , CN.Seq 34 1886
  , CN.Var 35 "__CLABEL_dispatch_opcode"
  , CN.Seq 35 36
  , CN.Branch 36 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 38 87
  , CN.Var 38 "NOP_38"
  , CN.Var 39 "__CLABEL_TARGET_BINARY_OP"
  , CN.Seq 39 40
  , CN.Assign 40 "undefed" (Num 0)
  , CN.Seq 40 41
  , CN.Assign 41 "next_instr" (Num 0)
  , CN.Seq 41 42
  , CN.Var 42 "__CLABEL_PREDICTED_BINARY_OP"
  , CN.Seq 42 43
  , CN.Var 43 "NOP_43"
  , CN.Var 44 "this_instr"
  , CN.Seq 44 45
  , CN.Var 45 "lhs"
  , CN.Seq 45 46
  , CN.Var 46 "rhs"
  , CN.Seq 46 47
  , CN.Var 47 "res"
  , CN.Seq 47 48
  , CN.Assign 48 "rhs" (Num 0)
  , CN.Seq 48 49
  , CN.Assign 49 "lhs" (Num 0)
  , CN.Seq 49 50
  , CN.Var 50 "counter"
  , CN.Seq 50 51
  , CN.Branch 51 (Eq (Num 0) (Num 1)) 53 56
  , CN.Assign 53 "next_instr" (Num 0)
  , CN.Seq 53 54
  , CN.Assign 54 "stack_pointer" (Num 0)
  , CN.Seq 54 55
  , CN.Assign 55 "opcode" (Num 0)
  , CN.Seq 55 56
  , CN.Seq 55 35
  , CN.Seq 55 57
  , CN.Var 56 "NOP_56"
  , CN.Seq 56 57
  , CN.Var 57 "IF_ELSE_FOOTER"
  , CN.Assign 58 "undefed" (Num 0)
  , CN.Seq 58 59
  , CN.Branch 59 (Eq (Num 0) (Num 1)) 60 61
  , CN.Assign 60 "undefed" (Num 0)
  , CN.Seq 60 59
  , CN.Var 61 "LOOP_FOOTER"
  , CN.Seq 61 62
  , CN.Var 62 "lhs_o"
  , CN.Seq 62 63
  , CN.Var 63 "rhs_o"
  , CN.Seq 63 64
  , CN.Var 64 "res_o"
  , CN.Seq 64 65
  , CN.Assign 65 "stack_pointer" (Num 0)
  , CN.Seq 65 66
  , CN.Branch 66 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 68 68
  , CN.Seq 67 1817
  , CN.Seq 67 69
  , CN.Var 68 "NOP_68"
  , CN.Seq 68 69
  , CN.Var 69 "IF_ELSE_FOOTER"
  , CN.Assign 70 "res" (Num 0)
  , CN.Seq 70 71
  , CN.Var 71 "tmp"
  , CN.Seq 71 72
  , CN.Assign 72 "lhs" (Num 0)
  , CN.Seq 72 73
  , CN.Assign 73 "undefed" (Num 0)
  , CN.Seq 73 74
  , CN.Assign 74 "tmp" (Num 0)
  , CN.Seq 74 75
  , CN.Assign 75 "rhs" (Num 0)
  , CN.Seq 75 76
  , CN.Assign 76 "undefed" (Num 0)
  , CN.Seq 76 77
  , CN.Assign 77 "stack_pointer" (Num 0)
  , CN.Seq 77 78
  , CN.Assign 78 "stack_pointer" (Num 0)
  , CN.Seq 78 79
  , CN.Var 79 "word"
  , CN.Seq 79 80
  , CN.Assign 80 "opcode" (Num 0)
  , CN.Seq 80 81
  , CN.Assign 81 "oparg" (Num 0)
  , CN.Seq 81 82
  , CN.Branch 82 (Eq (Num 0) (Num 1)) 83 86
  , CN.Var 83 "word"
  , CN.Seq 83 84
  , CN.Assign 84 "opcode" (Num 0)
  , CN.Seq 84 85
  , CN.Assign 85 "oparg" (Num 0)
  , CN.Seq 85 86
  , CN.Seq 85 82
  , CN.Var 86 "LOOP_FOOTER"
  , CN.Seq 86 87
  , CN.Seq 86 35
  , CN.Branch 87 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 89 129
  , CN.Var 89 "NOP_89"
  , CN.Var 90 "__CLABEL_TARGET_BINARY_OP_ADD_FLOAT"
  , CN.Seq 90 91
  , CN.Var 91 "this_instr"
  , CN.Seq 91 92
  , CN.Assign 92 "undefed" (Num 0)
  , CN.Seq 92 93
  , CN.Assign 93 "next_instr" (Num 0)
  , CN.Seq 93 94
  , CN.Var 94 "value"
  , CN.Seq 94 95
  , CN.Var 95 "left"
  , CN.Seq 95 96
  , CN.Var 96 "right"
  , CN.Seq 96 97
  , CN.Var 97 "res"
  , CN.Seq 97 98
  , CN.Assign 98 "value" (Num 0)
  , CN.Seq 98 99
  , CN.Var 99 "value_o"
  , CN.Seq 99 100
  , CN.Branch 100 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 102 102
  , CN.Seq 101 42
  , CN.Seq 101 103
  , CN.Var 102 "NOP_102"
  , CN.Seq 102 103
  , CN.Var 103 "IF_ELSE_FOOTER"
  , CN.Assign 104 "left" (Num 0)
  , CN.Seq 104 105
  , CN.Var 105 "left_o"
  , CN.Seq 105 106
  , CN.Branch 106 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 108 108
  , CN.Seq 107 42
  , CN.Seq 107 109
  , CN.Var 108 "NOP_108"
  , CN.Seq 108 109
  , CN.Var 109 "IF_ELSE_FOOTER"
  , CN.Assign 110 "right" (Num 0)
  , CN.Seq 110 111
  , CN.Var 111 "left_o"
  , CN.Seq 111 112
  , CN.Var 112 "right_o"
  , CN.Seq 112 113
  , CN.Var 113 "dres"
  , CN.Seq 113 114
  , CN.Assign 114 "res" (Num 0)
  , CN.Seq 114 115
  , CN.Branch 115 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 117 117
  , CN.Seq 116 1813
  , CN.Seq 116 118
  , CN.Var 117 "NOP_117"
  , CN.Seq 117 118
  , CN.Var 118 "IF_ELSE_FOOTER"
  , CN.Assign 119 "undefed" (Num 0)
  , CN.Seq 119 120
  , CN.Assign 120 "stack_pointer" (Num 0)
  , CN.Seq 120 121
  , CN.Var 121 "word"
  , CN.Seq 121 122
  , CN.Assign 122 "opcode" (Num 0)
  , CN.Seq 122 123
  , CN.Assign 123 "oparg" (Num 0)
  , CN.Seq 123 124
  , CN.Branch 124 (Eq (Num 0) (Num 1)) 125 128
  , CN.Var 125 "word"
  , CN.Seq 125 126
  , CN.Assign 126 "opcode" (Num 0)
  , CN.Seq 126 127
  , CN.Assign 127 "oparg" (Num 0)
  , CN.Seq 127 128
  , CN.Seq 127 124
  , CN.Var 128 "LOOP_FOOTER"
  , CN.Seq 128 129
  , CN.Seq 128 35
  , CN.Branch 129 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 131 170
  , CN.Var 131 "NOP_131"
  , CN.Var 132 "__CLABEL_TARGET_BINARY_OP_ADD_INT"
  , CN.Seq 132 133
  , CN.Var 133 "this_instr"
  , CN.Seq 133 134
  , CN.Assign 134 "undefed" (Num 0)
  , CN.Seq 134 135
  , CN.Assign 135 "next_instr" (Num 0)
  , CN.Seq 135 136
  , CN.Var 136 "value"
  , CN.Seq 136 137
  , CN.Var 137 "left"
  , CN.Seq 137 138
  , CN.Var 138 "right"
  , CN.Seq 138 139
  , CN.Var 139 "res"
  , CN.Seq 139 140
  , CN.Assign 140 "value" (Num 0)
  , CN.Seq 140 141
  , CN.Var 141 "value_o"
  , CN.Seq 141 142
  , CN.Branch 142 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 144 144
  , CN.Seq 143 42
  , CN.Seq 143 145
  , CN.Var 144 "NOP_144"
  , CN.Seq 144 145
  , CN.Var 145 "IF_ELSE_FOOTER"
  , CN.Assign 146 "left" (Num 0)
  , CN.Seq 146 147
  , CN.Var 147 "left_o"
  , CN.Seq 147 148
  , CN.Branch 148 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 150 150
  , CN.Seq 149 42
  , CN.Seq 149 151
  , CN.Var 150 "NOP_150"
  , CN.Seq 150 151
  , CN.Var 151 "IF_ELSE_FOOTER"
  , CN.Assign 152 "right" (Num 0)
  , CN.Seq 152 153
  , CN.Var 153 "left_o"
  , CN.Seq 153 154
  , CN.Var 154 "right_o"
  , CN.Seq 154 155
  , CN.Assign 155 "res" (Num 0)
  , CN.Seq 155 156
  , CN.Branch 156 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 158 158
  , CN.Seq 157 42
  , CN.Seq 157 159
  , CN.Var 158 "NOP_158"
  , CN.Seq 158 159
  , CN.Var 159 "IF_ELSE_FOOTER"
  , CN.Assign 160 "undefed" (Num 0)
  , CN.Seq 160 161
  , CN.Assign 161 "stack_pointer" (Num 0)
  , CN.Seq 161 162
  , CN.Var 162 "word"
  , CN.Seq 162 163
  , CN.Assign 163 "opcode" (Num 0)
  , CN.Seq 163 164
  , CN.Assign 164 "oparg" (Num 0)
  , CN.Seq 164 165
  , CN.Branch 165 (Eq (Num 0) (Num 1)) 166 169
  , CN.Var 166 "word"
  , CN.Seq 166 167
  , CN.Assign 167 "opcode" (Num 0)
  , CN.Seq 167 168
  , CN.Assign 168 "oparg" (Num 0)
  , CN.Seq 168 169
  , CN.Seq 168 165
  , CN.Var 169 "LOOP_FOOTER"
  , CN.Seq 169 170
  , CN.Seq 169 35
  , CN.Branch 170 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 172 214
  , CN.Var 172 "NOP_172"
  , CN.Var 173 "__CLABEL_TARGET_BINARY_OP_ADD_UNICODE"
  , CN.Seq 173 174
  , CN.Var 174 "this_instr"
  , CN.Seq 174 175
  , CN.Assign 175 "undefed" (Num 0)
  , CN.Seq 175 176
  , CN.Assign 176 "next_instr" (Num 0)
  , CN.Seq 176 177
  , CN.Var 177 "value"
  , CN.Seq 177 178
  , CN.Var 178 "nos"
  , CN.Seq 178 179
  , CN.Var 179 "left"
  , CN.Seq 179 180
  , CN.Var 180 "right"
  , CN.Seq 180 181
  , CN.Var 181 "res"
  , CN.Seq 181 182
  , CN.Assign 182 "value" (Num 0)
  , CN.Seq 182 183
  , CN.Var 183 "value_o"
  , CN.Seq 183 184
  , CN.Branch 184 (Eq (Plus (Plus (Num 0) (Plus (Id "PyUnicode_Type") (Num 0))) (Num 0)) (Num 1)) 186 186
  , CN.Seq 185 42
  , CN.Seq 185 187
  , CN.Var 186 "NOP_186"
  , CN.Seq 186 187
  , CN.Var 187 "IF_ELSE_FOOTER"
  , CN.Assign 188 "nos" (Num 0)
  , CN.Seq 188 189
  , CN.Var 189 "o"
  , CN.Seq 189 190
  , CN.Branch 190 (Eq (Plus (Plus (Num 0) (Plus (Id "PyUnicode_Type") (Num 0))) (Num 0)) (Num 1)) 192 192
  , CN.Seq 191 42
  , CN.Seq 191 193
  , CN.Var 192 "NOP_192"
  , CN.Seq 192 193
  , CN.Var 193 "IF_ELSE_FOOTER"
  , CN.Assign 194 "right" (Num 0)
  , CN.Seq 194 195
  , CN.Assign 195 "left" (Num 0)
  , CN.Seq 195 196
  , CN.Var 196 "left_o"
  , CN.Seq 196 197
  , CN.Var 197 "right_o"
  , CN.Seq 197 198
  , CN.Var 198 "res_o"
  , CN.Seq 198 199
  , CN.Branch 199 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 201 201
  , CN.Seq 200 1813
  , CN.Seq 200 202
  , CN.Var 201 "NOP_201"
  , CN.Seq 201 202
  , CN.Var 202 "IF_ELSE_FOOTER"
  , CN.Assign 203 "res" (Num 0)
  , CN.Seq 203 204
  , CN.Assign 204 "undefed" (Num 0)
  , CN.Seq 204 205
  , CN.Assign 205 "stack_pointer" (Num 0)
  , CN.Seq 205 206
  , CN.Var 206 "word"
  , CN.Seq 206 207
  , CN.Assign 207 "opcode" (Num 0)
  , CN.Seq 207 208
  , CN.Assign 208 "oparg" (Num 0)
  , CN.Seq 208 209
  , CN.Branch 209 (Eq (Num 0) (Num 1)) 210 213
  , CN.Var 210 "word"
  , CN.Seq 210 211
  , CN.Assign 211 "opcode" (Num 0)
  , CN.Seq 211 212
  , CN.Assign 212 "oparg" (Num 0)
  , CN.Seq 212 213
  , CN.Seq 212 209
  , CN.Var 213 "LOOP_FOOTER"
  , CN.Seq 213 214
  , CN.Seq 213 35
  , CN.Branch 214 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 216 260
  , CN.Var 216 "NOP_216"
  , CN.Var 217 "__CLABEL_TARGET_BINARY_OP_EXTEND"
  , CN.Seq 217 218
  , CN.Var 218 "this_instr"
  , CN.Seq 218 219
  , CN.Assign 219 "undefed" (Num 0)
  , CN.Seq 219 220
  , CN.Assign 220 "next_instr" (Num 0)
  , CN.Seq 220 221
  , CN.Var 221 "left"
  , CN.Seq 221 222
  , CN.Var 222 "right"
  , CN.Seq 222 223
  , CN.Var 223 "res"
  , CN.Seq 223 224
  , CN.Assign 224 "right" (Num 0)
  , CN.Seq 224 225
  , CN.Assign 225 "left" (Num 0)
  , CN.Seq 225 226
  , CN.Var 226 "descr"
  , CN.Seq 226 227
  , CN.Var 227 "left_o"
  , CN.Seq 227 228
  , CN.Var 228 "right_o"
  , CN.Seq 228 229
  , CN.Var 229 "d"
  , CN.Seq 229 230
  , CN.Var 230 "res"
  , CN.Seq 230 231
  , CN.Assign 231 "stack_pointer" (Num 0)
  , CN.Seq 231 232
  , CN.Branch 232 (Eq (Plus (Id "res") (Num 0)) (Num 1)) 234 234
  , CN.Seq 233 42
  , CN.Seq 233 235
  , CN.Var 234 "NOP_234"
  , CN.Seq 234 235
  , CN.Var 235 "IF_ELSE_FOOTER"
  , CN.Var 236 "descr"
  , CN.Seq 236 237
  , CN.Var 237 "left_o"
  , CN.Seq 237 238
  , CN.Var 238 "right_o"
  , CN.Seq 238 239
  , CN.Var 239 "d"
  , CN.Seq 239 240
  , CN.Var 240 "res_o"
  , CN.Seq 240 241
  , CN.Var 241 "tmp"
  , CN.Seq 241 242
  , CN.Assign 242 "right" (Num 0)
  , CN.Seq 242 243
  , CN.Assign 243 "undefed" (Num 0)
  , CN.Seq 243 244
  , CN.Assign 244 "tmp" (Num 0)
  , CN.Seq 244 245
  , CN.Assign 245 "left" (Num 0)
  , CN.Seq 245 246
  , CN.Assign 246 "undefed" (Num 0)
  , CN.Seq 246 247
  , CN.Assign 247 "stack_pointer" (Num 0)
  , CN.Seq 247 248
  , CN.Assign 248 "stack_pointer" (Num 0)
  , CN.Seq 248 249
  , CN.Assign 249 "res" (Num 0)
  , CN.Seq 249 250
  , CN.Assign 250 "undefed" (Num 0)
  , CN.Seq 250 251
  , CN.Assign 251 "stack_pointer" (Num 0)
  , CN.Seq 251 252
  , CN.Var 252 "word"
  , CN.Seq 252 253
  , CN.Assign 253 "opcode" (Num 0)
  , CN.Seq 253 254
  , CN.Assign 254 "oparg" (Num 0)
  , CN.Seq 254 255
  , CN.Branch 255 (Eq (Num 0) (Num 1)) 256 259
  , CN.Var 256 "word"
  , CN.Seq 256 257
  , CN.Assign 257 "opcode" (Num 0)
  , CN.Seq 257 258
  , CN.Assign 258 "oparg" (Num 0)
  , CN.Seq 258 259
  , CN.Seq 258 255
  , CN.Var 259 "LOOP_FOOTER"
  , CN.Seq 259 260
  , CN.Seq 259 35
  , CN.Branch 260 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 262 354
  , CN.Var 262 "NOP_262"
  , CN.Var 263 "__CLABEL_TARGET_BINARY_OP_INPLACE_ADD_UNICODE"
  , CN.Seq 263 264
  , CN.Var 264 "this_instr"
  , CN.Seq 264 265
  , CN.Assign 265 "undefed" (Num 0)
  , CN.Seq 265 266
  , CN.Assign 266 "next_instr" (Num 0)
  , CN.Seq 266 267
  , CN.Var 267 "value"
  , CN.Seq 267 268
  , CN.Var 268 "nos"
  , CN.Seq 268 269
  , CN.Var 269 "left"
  , CN.Seq 269 270
  , CN.Var 270 "right"
  , CN.Seq 270 271
  , CN.Assign 271 "value" (Num 0)
  , CN.Seq 271 272
  , CN.Var 272 "value_o"
  , CN.Seq 272 273
  , CN.Branch 273 (Eq (Plus (Plus (Num 0) (Plus (Id "PyUnicode_Type") (Num 0))) (Num 0)) (Num 1)) 275 275
  , CN.Seq 274 42
  , CN.Seq 274 276
  , CN.Var 275 "NOP_275"
  , CN.Seq 275 276
  , CN.Var 276 "IF_ELSE_FOOTER"
  , CN.Assign 277 "nos" (Num 0)
  , CN.Seq 277 278
  , CN.Var 278 "o"
  , CN.Seq 278 279
  , CN.Branch 279 (Eq (Plus (Plus (Num 0) (Plus (Id "PyUnicode_Type") (Num 0))) (Num 0)) (Num 1)) 281 281
  , CN.Seq 280 42
  , CN.Seq 280 282
  , CN.Var 281 "NOP_281"
  , CN.Seq 281 282
  , CN.Var 282 "IF_ELSE_FOOTER"
  , CN.Assign 283 "right" (Num 0)
  , CN.Seq 283 284
  , CN.Assign 284 "left" (Num 0)
  , CN.Seq 284 285
  , CN.Var 285 "left_o"
  , CN.Seq 285 286
  , CN.Var 286 "next_oparg"
  , CN.Seq 286 287
  , CN.Assign 287 "next_oparg" (Num 0)
  , CN.Seq 287 288
  , CN.Var 288 "target_local"
  , CN.Seq 288 289
  , CN.Branch 289 (Eq (Plus (Num 0) (Id "left_o")) (Num 1)) 291 291
  , CN.Seq 290 42
  , CN.Seq 290 292
  , CN.Var 291 "NOP_291"
  , CN.Seq 291 292
  , CN.Var 292 "IF_ELSE_FOOTER"
  , CN.Var 293 "temp"
  , CN.Seq 293 294
  , CN.Var 294 "right_o"
  , CN.Seq 294 295
  , CN.Assign 295 "stack_pointer" (Num 0)
  , CN.Seq 295 296
  , CN.Assign 296 "stack_pointer" (Num 0)
  , CN.Seq 296 297
  , CN.Assign 297 "undefed" (Num 0)
  , CN.Seq 297 298
  , CN.Var 298 "op"
  , CN.Seq 298 299
  , CN.Branch 299 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 301 316
  , CN.Var 301 "tracer"
  , CN.Seq 301 302
  , CN.Branch 302 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 304 305
  , CN.Var 304 "data"
  , CN.Seq 304 305
  , CN.Seq 304 306
  , CN.Var 305 "NOP_305"
  , CN.Seq 305 306
  , CN.Var 306 "IF_ELSE_FOOTER"
  , CN.Branch 307 (Eq (Num 0) (Num 1)) 308 314
  , CN.Var 308 "tracer"
  , CN.Seq 308 309
  , CN.Branch 309 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 311 312
  , CN.Var 311 "data"
  , CN.Seq 311 312
  , CN.Seq 311 313
  , CN.Var 312 "NOP_312"
  , CN.Seq 312 313
  , CN.Var 313 "IF_ELSE_FOOTER"
  , CN.Seq 313 307
  , CN.Var 314 "LOOP_FOOTER"
  , CN.Seq 314 315
  , CN.Var 315 "dealloc"
  , CN.Seq 315 316
  , CN.Seq 315 317
  , CN.Var 316 "NOP_316"
  , CN.Seq 316 317
  , CN.Var 317 "IF_ELSE_FOOTER"
  , CN.Branch 318 (Eq (Num 0) (Num 1)) 319 339
  , CN.Var 319 "op"
  , CN.Seq 319 320
  , CN.Branch 320 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 322 337
  , CN.Var 322 "tracer"
  , CN.Seq 322 323
  , CN.Branch 323 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 325 326
  , CN.Var 325 "data"
  , CN.Seq 325 326
  , CN.Seq 325 327
  , CN.Var 326 "NOP_326"
  , CN.Seq 326 327
  , CN.Var 327 "IF_ELSE_FOOTER"
  , CN.Branch 328 (Eq (Num 0) (Num 1)) 329 335
  , CN.Var 329 "tracer"
  , CN.Seq 329 330
  , CN.Branch 330 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 332 333
  , CN.Var 332 "data"
  , CN.Seq 332 333
  , CN.Seq 332 334
  , CN.Var 333 "NOP_333"
  , CN.Seq 333 334
  , CN.Var 334 "IF_ELSE_FOOTER"
  , CN.Seq 334 328
  , CN.Var 335 "LOOP_FOOTER"
  , CN.Seq 335 336
  , CN.Var 336 "dealloc"
  , CN.Seq 336 337
  , CN.Seq 336 338
  , CN.Var 337 "NOP_337"
  , CN.Seq 337 338
  , CN.Var 338 "IF_ELSE_FOOTER"
  , CN.Seq 338 318
  , CN.Var 339 "LOOP_FOOTER"
  , CN.Seq 339 340
  , CN.Assign 340 "stack_pointer" (Num 0)
  , CN.Seq 340 341
  , CN.Branch 341 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 343 343
  , CN.Seq 342 1817
  , CN.Seq 342 344
  , CN.Var 343 "NOP_343"
  , CN.Seq 343 344
  , CN.Var 344 "IF_ELSE_FOOTER"
  , CN.Assign 345 "next_instr" (Num 0)
  , CN.Seq 345 346
  , CN.Var 346 "word"
  , CN.Seq 346 347
  , CN.Assign 347 "opcode" (Num 0)
  , CN.Seq 347 348
  , CN.Assign 348 "oparg" (Num 0)
  , CN.Seq 348 349
  , CN.Branch 349 (Eq (Num 0) (Num 1)) 350 353
  , CN.Var 350 "word"
  , CN.Seq 350 351
  , CN.Assign 351 "opcode" (Num 0)
  , CN.Seq 351 352
  , CN.Assign 352 "oparg" (Num 0)
  , CN.Seq 352 353
  , CN.Seq 352 349
  , CN.Var 353 "LOOP_FOOTER"
  , CN.Seq 353 354
  , CN.Seq 353 35
  , CN.Branch 354 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 356 396
  , CN.Var 356 "NOP_356"
  , CN.Var 357 "__CLABEL_TARGET_BINARY_OP_MULTIPLY_FLOAT"
  , CN.Seq 357 358
  , CN.Var 358 "this_instr"
  , CN.Seq 358 359
  , CN.Assign 359 "undefed" (Num 0)
  , CN.Seq 359 360
  , CN.Assign 360 "next_instr" (Num 0)
  , CN.Seq 360 361
  , CN.Var 361 "value"
  , CN.Seq 361 362
  , CN.Var 362 "left"
  , CN.Seq 362 363
  , CN.Var 363 "right"
  , CN.Seq 363 364
  , CN.Var 364 "res"
  , CN.Seq 364 365
  , CN.Assign 365 "value" (Num 0)
  , CN.Seq 365 366
  , CN.Var 366 "value_o"
  , CN.Seq 366 367
  , CN.Branch 367 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 369 369
  , CN.Seq 368 42
  , CN.Seq 368 370
  , CN.Var 369 "NOP_369"
  , CN.Seq 369 370
  , CN.Var 370 "IF_ELSE_FOOTER"
  , CN.Assign 371 "left" (Num 0)
  , CN.Seq 371 372
  , CN.Var 372 "left_o"
  , CN.Seq 372 373
  , CN.Branch 373 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 375 375
  , CN.Seq 374 42
  , CN.Seq 374 376
  , CN.Var 375 "NOP_375"
  , CN.Seq 375 376
  , CN.Var 376 "IF_ELSE_FOOTER"
  , CN.Assign 377 "right" (Num 0)
  , CN.Seq 377 378
  , CN.Var 378 "left_o"
  , CN.Seq 378 379
  , CN.Var 379 "right_o"
  , CN.Seq 379 380
  , CN.Var 380 "dres"
  , CN.Seq 380 381
  , CN.Assign 381 "res" (Num 0)
  , CN.Seq 381 382
  , CN.Branch 382 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 384 384
  , CN.Seq 383 1813
  , CN.Seq 383 385
  , CN.Var 384 "NOP_384"
  , CN.Seq 384 385
  , CN.Var 385 "IF_ELSE_FOOTER"
  , CN.Assign 386 "undefed" (Num 0)
  , CN.Seq 386 387
  , CN.Assign 387 "stack_pointer" (Num 0)
  , CN.Seq 387 388
  , CN.Var 388 "word"
  , CN.Seq 388 389
  , CN.Assign 389 "opcode" (Num 0)
  , CN.Seq 389 390
  , CN.Assign 390 "oparg" (Num 0)
  , CN.Seq 390 391
  , CN.Branch 391 (Eq (Num 0) (Num 1)) 392 395
  , CN.Var 392 "word"
  , CN.Seq 392 393
  , CN.Assign 393 "opcode" (Num 0)
  , CN.Seq 393 394
  , CN.Assign 394 "oparg" (Num 0)
  , CN.Seq 394 395
  , CN.Seq 394 391
  , CN.Var 395 "LOOP_FOOTER"
  , CN.Seq 395 396
  , CN.Seq 395 35
  , CN.Branch 396 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 398 437
  , CN.Var 398 "NOP_398"
  , CN.Var 399 "__CLABEL_TARGET_BINARY_OP_MULTIPLY_INT"
  , CN.Seq 399 400
  , CN.Var 400 "this_instr"
  , CN.Seq 400 401
  , CN.Assign 401 "undefed" (Num 0)
  , CN.Seq 401 402
  , CN.Assign 402 "next_instr" (Num 0)
  , CN.Seq 402 403
  , CN.Var 403 "value"
  , CN.Seq 403 404
  , CN.Var 404 "left"
  , CN.Seq 404 405
  , CN.Var 405 "right"
  , CN.Seq 405 406
  , CN.Var 406 "res"
  , CN.Seq 406 407
  , CN.Assign 407 "value" (Num 0)
  , CN.Seq 407 408
  , CN.Var 408 "value_o"
  , CN.Seq 408 409
  , CN.Branch 409 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 411 411
  , CN.Seq 410 42
  , CN.Seq 410 412
  , CN.Var 411 "NOP_411"
  , CN.Seq 411 412
  , CN.Var 412 "IF_ELSE_FOOTER"
  , CN.Assign 413 "left" (Num 0)
  , CN.Seq 413 414
  , CN.Var 414 "left_o"
  , CN.Seq 414 415
  , CN.Branch 415 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 417 417
  , CN.Seq 416 42
  , CN.Seq 416 418
  , CN.Var 417 "NOP_417"
  , CN.Seq 417 418
  , CN.Var 418 "IF_ELSE_FOOTER"
  , CN.Assign 419 "right" (Num 0)
  , CN.Seq 419 420
  , CN.Var 420 "left_o"
  , CN.Seq 420 421
  , CN.Var 421 "right_o"
  , CN.Seq 421 422
  , CN.Assign 422 "res" (Num 0)
  , CN.Seq 422 423
  , CN.Branch 423 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 425 425
  , CN.Seq 424 42
  , CN.Seq 424 426
  , CN.Var 425 "NOP_425"
  , CN.Seq 425 426
  , CN.Var 426 "IF_ELSE_FOOTER"
  , CN.Assign 427 "undefed" (Num 0)
  , CN.Seq 427 428
  , CN.Assign 428 "stack_pointer" (Num 0)
  , CN.Seq 428 429
  , CN.Var 429 "word"
  , CN.Seq 429 430
  , CN.Assign 430 "opcode" (Num 0)
  , CN.Seq 430 431
  , CN.Assign 431 "oparg" (Num 0)
  , CN.Seq 431 432
  , CN.Branch 432 (Eq (Num 0) (Num 1)) 433 436
  , CN.Var 433 "word"
  , CN.Seq 433 434
  , CN.Assign 434 "opcode" (Num 0)
  , CN.Seq 434 435
  , CN.Assign 435 "oparg" (Num 0)
  , CN.Seq 435 436
  , CN.Seq 435 432
  , CN.Var 436 "LOOP_FOOTER"
  , CN.Seq 436 437
  , CN.Seq 436 35
  , CN.Branch 437 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 439 489
  , CN.Var 439 "NOP_439"
  , CN.Var 440 "__CLABEL_TARGET_BINARY_OP_SUBSCR_DICT"
  , CN.Seq 440 441
  , CN.Var 441 "this_instr"
  , CN.Seq 441 442
  , CN.Assign 442 "undefed" (Num 0)
  , CN.Seq 442 443
  , CN.Assign 443 "next_instr" (Num 0)
  , CN.Seq 443 444
  , CN.Var 444 "nos"
  , CN.Seq 444 445
  , CN.Var 445 "dict_st"
  , CN.Seq 445 446
  , CN.Var 446 "sub_st"
  , CN.Seq 446 447
  , CN.Var 447 "res"
  , CN.Seq 447 448
  , CN.Assign 448 "nos" (Num 0)
  , CN.Seq 448 449
  , CN.Var 449 "o"
  , CN.Seq 449 450
  , CN.Branch 450 (Eq (Plus (Plus (Num 0) (Plus (Id "PyDict_Type") (Num 0))) (Num 0)) (Num 1)) 452 452
  , CN.Seq 451 42
  , CN.Seq 451 453
  , CN.Var 452 "NOP_452"
  , CN.Seq 452 453
  , CN.Var 453 "IF_ELSE_FOOTER"
  , CN.Assign 454 "sub_st" (Num 0)
  , CN.Seq 454 455
  , CN.Assign 455 "dict_st" (Num 0)
  , CN.Seq 455 456
  , CN.Var 456 "sub"
  , CN.Seq 456 457
  , CN.Var 457 "dict"
  , CN.Seq 457 458
  , CN.Var 458 "res_o"
  , CN.Seq 458 459
  , CN.Var 459 "rc"
  , CN.Seq 459 460
  , CN.Assign 460 "stack_pointer" (Num 0)
  , CN.Seq 460 461
  , CN.Branch 461 (Eq (Plus (Id "rc") (Num 0)) (Num 1)) 463 464
  , CN.Assign 463 "stack_pointer" (Num 0)
  , CN.Seq 463 465
  , CN.Var 464 "NOP_464"
  , CN.Seq 464 465
  , CN.Var 465 "IF_ELSE_FOOTER"
  , CN.Var 466 "tmp"
  , CN.Seq 466 467
  , CN.Assign 467 "sub_st" (Num 0)
  , CN.Seq 467 468
  , CN.Assign 468 "undefed" (Num 0)
  , CN.Seq 468 469
  , CN.Assign 469 "tmp" (Num 0)
  , CN.Seq 469 470
  , CN.Assign 470 "dict_st" (Num 0)
  , CN.Seq 470 471
  , CN.Assign 471 "undefed" (Num 0)
  , CN.Seq 471 472
  , CN.Assign 472 "stack_pointer" (Num 0)
  , CN.Seq 472 473
  , CN.Assign 473 "stack_pointer" (Num 0)
  , CN.Seq 473 474
  , CN.Branch 474 (Eq (Plus (Id "rc") (Num 0)) (Num 1)) 476 476
  , CN.Seq 475 1817
  , CN.Seq 475 477
  , CN.Var 476 "NOP_476"
  , CN.Seq 476 477
  , CN.Var 477 "IF_ELSE_FOOTER"
  , CN.Assign 478 "res" (Num 0)
  , CN.Seq 478 479
  , CN.Assign 479 "undefed" (Num 0)
  , CN.Seq 479 480
  , CN.Assign 480 "stack_pointer" (Num 0)
  , CN.Seq 480 481
  , CN.Var 481 "word"
  , CN.Seq 481 482
  , CN.Assign 482 "opcode" (Num 0)
  , CN.Seq 482 483
  , CN.Assign 483 "oparg" (Num 0)
  , CN.Seq 483 484
  , CN.Branch 484 (Eq (Num 0) (Num 1)) 485 488
  , CN.Var 485 "word"
  , CN.Seq 485 486
  , CN.Assign 486 "opcode" (Num 0)
  , CN.Seq 486 487
  , CN.Assign 487 "oparg" (Num 0)
  , CN.Seq 487 488
  , CN.Seq 487 484
  , CN.Var 488 "LOOP_FOOTER"
  , CN.Seq 488 489
  , CN.Seq 488 35
  , CN.Branch 489 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 491 549
  , CN.Var 491 "NOP_491"
  , CN.Var 492 "__CLABEL_TARGET_BINARY_OP_SUBSCR_GETITEM"
  , CN.Seq 492 493
  , CN.Var 493 "this_instr"
  , CN.Seq 493 494
  , CN.Assign 494 "undefed" (Num 0)
  , CN.Seq 494 495
  , CN.Assign 495 "next_instr" (Num 0)
  , CN.Seq 495 496
  , CN.Var 496 "container"
  , CN.Seq 496 497
  , CN.Var 497 "getitem"
  , CN.Seq 497 498
  , CN.Var 498 "sub"
  , CN.Seq 498 499
  , CN.Var 499 "new_frame"
  , CN.Seq 499 500
  , CN.Branch 500 (Eq (Num 0) (Num 1)) 502 502
  , CN.Seq 501 42
  , CN.Seq 501 503
  , CN.Var 502 "NOP_502"
  , CN.Seq 502 503
  , CN.Var 503 "IF_ELSE_FOOTER"
  , CN.Assign 504 "container" (Num 0)
  , CN.Seq 504 505
  , CN.Var 505 "tp"
  , CN.Seq 505 506
  , CN.Branch 506 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 508 508
  , CN.Seq 507 42
  , CN.Seq 507 509
  , CN.Var 508 "NOP_508"
  , CN.Seq 508 509
  , CN.Var 509 "IF_ELSE_FOOTER"
  , CN.Var 510 "ht"
  , CN.Seq 510 511
  , CN.Var 511 "getitem_o"
  , CN.Seq 511 512
  , CN.Branch 512 (Eq (Plus (Id "getitem_o") (Num 0)) (Num 1)) 514 514
  , CN.Seq 513 42
  , CN.Seq 513 515
  , CN.Var 514 "NOP_514"
  , CN.Seq 514 515
  , CN.Var 515 "IF_ELSE_FOOTER"
  , CN.Var 516 "cached_version"
  , CN.Seq 516 517
  , CN.Branch 517 (Eq (Plus (Num 0) (Id "cached_version")) (Num 1)) 519 519
  , CN.Seq 518 42
  , CN.Seq 518 520
  , CN.Var 519 "NOP_519"
  , CN.Seq 519 520
  , CN.Var 520 "IF_ELSE_FOOTER"
  , CN.Var 521 "code"
  , CN.Seq 521 522
  , CN.Branch 522 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 524 524
  , CN.Seq 523 42
  , CN.Seq 523 525
  , CN.Var 524 "NOP_524"
  , CN.Seq 524 525
  , CN.Var 525 "IF_ELSE_FOOTER"
  , CN.Assign 526 "getitem" (Num 0)
  , CN.Seq 526 527
  , CN.Assign 527 "sub" (Num 0)
  , CN.Seq 527 528
  , CN.Var 528 "pushed_frame"
  , CN.Seq 528 529
  , CN.Assign 529 "undefed" (Num 0)
  , CN.Seq 529 530
  , CN.Assign 530 "undefed" (Num 0)
  , CN.Seq 530 531
  , CN.Assign 531 "undefed" (Num 0)
  , CN.Seq 531 532
  , CN.Assign 532 "new_frame" (Num 0)
  , CN.Seq 532 533
  , CN.Var 533 "temp"
  , CN.Seq 533 534
  , CN.Assign 534 "stack_pointer" (Num 0)
  , CN.Seq 534 535
  , CN.Assign 535 "frame" (Num 0)
  , CN.Seq 535 536
  , CN.Assign 536 "stack_pointer" (Num 0)
  , CN.Seq 536 537
  , CN.Assign 537 "next_instr" (Num 0)
  , CN.Seq 537 538
  , CN.Branch 538 (Eq (Num 0) (Num 1)) 539 540
  , CN.Assign 539 "next_instr" (Num 0)
  , CN.Seq 539 538
  , CN.Var 540 "LOOP_FOOTER"
  , CN.Seq 540 541
  , CN.Var 541 "word"
  , CN.Seq 541 542
  , CN.Assign 542 "opcode" (Num 0)
  , CN.Seq 542 543
  , CN.Assign 543 "oparg" (Num 0)
  , CN.Seq 543 544
  , CN.Branch 544 (Eq (Num 0) (Num 1)) 545 548
  , CN.Var 545 "word"
  , CN.Seq 545 546
  , CN.Assign 546 "opcode" (Num 0)
  , CN.Seq 546 547
  , CN.Assign 547 "oparg" (Num 0)
  , CN.Seq 547 548
  , CN.Seq 547 544
  , CN.Var 548 "LOOP_FOOTER"
  , CN.Seq 548 549
  , CN.Seq 548 35
  , CN.Branch 549 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 551 604
  , CN.Var 551 "NOP_551"
  , CN.Var 552 "__CLABEL_TARGET_BINARY_OP_SUBSCR_LIST_INT"
  , CN.Seq 552 553
  , CN.Var 553 "this_instr"
  , CN.Seq 553 554
  , CN.Assign 554 "undefed" (Num 0)
  , CN.Seq 554 555
  , CN.Assign 555 "next_instr" (Num 0)
  , CN.Seq 555 556
  , CN.Var 556 "value"
  , CN.Seq 556 557
  , CN.Var 557 "nos"
  , CN.Seq 557 558
  , CN.Var 558 "list_st"
  , CN.Seq 558 559
  , CN.Var 559 "sub_st"
  , CN.Seq 559 560
  , CN.Var 560 "res"
  , CN.Seq 560 561
  , CN.Assign 561 "value" (Num 0)
  , CN.Seq 561 562
  , CN.Var 562 "value_o"
  , CN.Seq 562 563
  , CN.Branch 563 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 565 565
  , CN.Seq 564 42
  , CN.Seq 564 566
  , CN.Var 565 "NOP_565"
  , CN.Seq 565 566
  , CN.Var 566 "IF_ELSE_FOOTER"
  , CN.Assign 567 "nos" (Num 0)
  , CN.Seq 567 568
  , CN.Var 568 "o"
  , CN.Seq 568 569
  , CN.Branch 569 (Eq (Plus (Plus (Num 0) (Plus (Id "PyList_Type") (Num 0))) (Num 0)) (Num 1)) 571 571
  , CN.Seq 570 42
  , CN.Seq 570 572
  , CN.Var 571 "NOP_571"
  , CN.Seq 571 572
  , CN.Var 572 "IF_ELSE_FOOTER"
  , CN.Assign 573 "sub_st" (Num 0)
  , CN.Seq 573 574
  , CN.Assign 574 "list_st" (Num 0)
  , CN.Seq 574 575
  , CN.Var 575 "sub"
  , CN.Seq 575 576
  , CN.Var 576 "list"
  , CN.Seq 576 577
  , CN.Branch 577 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 579 579
  , CN.Seq 578 42
  , CN.Seq 578 580
  , CN.Var 579 "NOP_579"
  , CN.Seq 579 580
  , CN.Var 580 "IF_ELSE_FOOTER"
  , CN.Var 581 "index"
  , CN.Seq 581 582
  , CN.Branch 582 (Eq (Plus (Id "index") (Num 0)) (Num 1)) 584 584
  , CN.Seq 583 42
  , CN.Seq 583 585
  , CN.Var 584 "NOP_584"
  , CN.Seq 584 585
  , CN.Var 585 "IF_ELSE_FOOTER"
  , CN.Var 586 "res_o"
  , CN.Seq 586 587
  , CN.Assign 587 "res" (Num 0)
  , CN.Seq 587 588
  , CN.Var 588 "tmp"
  , CN.Seq 588 589
  , CN.Assign 589 "list_st" (Num 0)
  , CN.Seq 589 590
  , CN.Assign 590 "undefed" (Num 0)
  , CN.Seq 590 591
  , CN.Assign 591 "tmp" (Num 0)
  , CN.Seq 591 592
  , CN.Assign 592 "sub_st" (Num 0)
  , CN.Seq 592 593
  , CN.Assign 593 "undefed" (Num 0)
  , CN.Seq 593 594
  , CN.Assign 594 "stack_pointer" (Num 0)
  , CN.Seq 594 595
  , CN.Assign 595 "stack_pointer" (Num 0)
  , CN.Seq 595 596
  , CN.Var 596 "word"
  , CN.Seq 596 597
  , CN.Assign 597 "opcode" (Num 0)
  , CN.Seq 597 598
  , CN.Assign 598 "oparg" (Num 0)
  , CN.Seq 598 599
  , CN.Branch 599 (Eq (Num 0) (Num 1)) 600 603
  , CN.Var 600 "word"
  , CN.Seq 600 601
  , CN.Assign 601 "opcode" (Num 0)
  , CN.Seq 601 602
  , CN.Assign 602 "oparg" (Num 0)
  , CN.Seq 602 603
  , CN.Seq 602 599
  , CN.Var 603 "LOOP_FOOTER"
  , CN.Seq 603 604
  , CN.Seq 603 35
  , CN.Branch 604 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 606 657
  , CN.Var 606 "NOP_606"
  , CN.Var 607 "__CLABEL_TARGET_BINARY_OP_SUBSCR_LIST_SLICE"
  , CN.Seq 607 608
  , CN.Var 608 "this_instr"
  , CN.Seq 608 609
  , CN.Assign 609 "undefed" (Num 0)
  , CN.Seq 609 610
  , CN.Assign 610 "next_instr" (Num 0)
  , CN.Seq 610 611
  , CN.Var 611 "tos"
  , CN.Seq 611 612
  , CN.Var 612 "nos"
  , CN.Seq 612 613
  , CN.Var 613 "list_st"
  , CN.Seq 613 614
  , CN.Var 614 "sub_st"
  , CN.Seq 614 615
  , CN.Var 615 "res"
  , CN.Seq 615 616
  , CN.Assign 616 "tos" (Num 0)
  , CN.Seq 616 617
  , CN.Var 617 "o"
  , CN.Seq 617 618
  , CN.Branch 618 (Eq (Plus (Plus (Num 0) (Plus (Id "PySlice_Type") (Num 0))) (Num 0)) (Num 1)) 620 620
  , CN.Seq 619 42
  , CN.Seq 619 621
  , CN.Var 620 "NOP_620"
  , CN.Seq 620 621
  , CN.Var 621 "IF_ELSE_FOOTER"
  , CN.Assign 622 "nos" (Num 0)
  , CN.Seq 622 623
  , CN.Var 623 "o"
  , CN.Seq 623 624
  , CN.Branch 624 (Eq (Plus (Plus (Num 0) (Plus (Id "PyList_Type") (Num 0))) (Num 0)) (Num 1)) 626 626
  , CN.Seq 625 42
  , CN.Seq 625 627
  , CN.Var 626 "NOP_626"
  , CN.Seq 626 627
  , CN.Var 627 "IF_ELSE_FOOTER"
  , CN.Assign 628 "sub_st" (Num 0)
  , CN.Seq 628 629
  , CN.Assign 629 "list_st" (Num 0)
  , CN.Seq 629 630
  , CN.Var 630 "sub"
  , CN.Seq 630 631
  , CN.Var 631 "list"
  , CN.Seq 631 632
  , CN.Var 632 "res_o"
  , CN.Seq 632 633
  , CN.Assign 633 "stack_pointer" (Num 0)
  , CN.Seq 633 634
  , CN.Var 634 "tmp"
  , CN.Seq 634 635
  , CN.Assign 635 "sub_st" (Num 0)
  , CN.Seq 635 636
  , CN.Assign 636 "undefed" (Num 0)
  , CN.Seq 636 637
  , CN.Assign 637 "tmp" (Num 0)
  , CN.Seq 637 638
  , CN.Assign 638 "list_st" (Num 0)
  , CN.Seq 638 639
  , CN.Assign 639 "undefed" (Num 0)
  , CN.Seq 639 640
  , CN.Assign 640 "stack_pointer" (Num 0)
  , CN.Seq 640 641
  , CN.Assign 641 "stack_pointer" (Num 0)
  , CN.Seq 641 642
  , CN.Branch 642 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 644 644
  , CN.Seq 643 1817
  , CN.Seq 643 645
  , CN.Var 644 "NOP_644"
  , CN.Seq 644 645
  , CN.Var 645 "IF_ELSE_FOOTER"
  , CN.Assign 646 "res" (Num 0)
  , CN.Seq 646 647
  , CN.Assign 647 "undefed" (Num 0)
  , CN.Seq 647 648
  , CN.Assign 648 "stack_pointer" (Num 0)
  , CN.Seq 648 649
  , CN.Var 649 "word"
  , CN.Seq 649 650
  , CN.Assign 650 "opcode" (Num 0)
  , CN.Seq 650 651
  , CN.Assign 651 "oparg" (Num 0)
  , CN.Seq 651 652
  , CN.Branch 652 (Eq (Num 0) (Num 1)) 653 656
  , CN.Var 653 "word"
  , CN.Seq 653 654
  , CN.Assign 654 "opcode" (Num 0)
  , CN.Seq 654 655
  , CN.Assign 655 "oparg" (Num 0)
  , CN.Seq 655 656
  , CN.Seq 655 652
  , CN.Var 656 "LOOP_FOOTER"
  , CN.Seq 656 657
  , CN.Seq 656 35
  , CN.Branch 657 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 659 713
  , CN.Var 659 "NOP_659"
  , CN.Var 660 "__CLABEL_TARGET_BINARY_OP_SUBSCR_STR_INT"
  , CN.Seq 660 661
  , CN.Var 661 "this_instr"
  , CN.Seq 661 662
  , CN.Assign 662 "undefed" (Num 0)
  , CN.Seq 662 663
  , CN.Assign 663 "next_instr" (Num 0)
  , CN.Seq 663 664
  , CN.Var 664 "value"
  , CN.Seq 664 665
  , CN.Var 665 "nos"
  , CN.Seq 665 666
  , CN.Var 666 "str_st"
  , CN.Seq 666 667
  , CN.Var 667 "sub_st"
  , CN.Seq 667 668
  , CN.Var 668 "res"
  , CN.Seq 668 669
  , CN.Assign 669 "value" (Num 0)
  , CN.Seq 669 670
  , CN.Var 670 "value_o"
  , CN.Seq 670 671
  , CN.Branch 671 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 673 673
  , CN.Seq 672 42
  , CN.Seq 672 674
  , CN.Var 673 "NOP_673"
  , CN.Seq 673 674
  , CN.Var 674 "IF_ELSE_FOOTER"
  , CN.Assign 675 "nos" (Num 0)
  , CN.Seq 675 676
  , CN.Var 676 "o"
  , CN.Seq 676 677
  , CN.Branch 677 (Eq (Plus (Plus (Num 0) (Plus (Id "PyUnicode_Type") (Num 0))) (Num 0)) (Num 1)) 679 679
  , CN.Seq 678 42
  , CN.Seq 678 680
  , CN.Var 679 "NOP_679"
  , CN.Seq 679 680
  , CN.Var 680 "IF_ELSE_FOOTER"
  , CN.Assign 681 "sub_st" (Num 0)
  , CN.Seq 681 682
  , CN.Assign 682 "str_st" (Num 0)
  , CN.Seq 682 683
  , CN.Var 683 "sub"
  , CN.Seq 683 684
  , CN.Var 684 "str"
  , CN.Seq 684 685
  , CN.Branch 685 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 687 687
  , CN.Seq 686 42
  , CN.Seq 686 688
  , CN.Var 687 "NOP_687"
  , CN.Seq 687 688
  , CN.Var 688 "IF_ELSE_FOOTER"
  , CN.Var 689 "index"
  , CN.Seq 689 690
  , CN.Branch 690 (Eq (Plus (Num 0) (Id "index")) (Num 1)) 692 692
  , CN.Seq 691 42
  , CN.Seq 691 693
  , CN.Var 692 "NOP_692"
  , CN.Seq 692 693
  , CN.Var 693 "IF_ELSE_FOOTER"
  , CN.Var 694 "c"
  , CN.Seq 694 695
  , CN.Branch 695 (Eq (Plus (Plus (Plus (Num 0) (Num 0)) (Plus (Num 0) (Num 0))) (Id "c")) (Num 1)) 697 697
  , CN.Seq 696 42
  , CN.Seq 696 698
  , CN.Var 697 "NOP_697"
  , CN.Seq 697 698
  , CN.Var 698 "IF_ELSE_FOOTER"
  , CN.Var 699 "res_o"
  , CN.Seq 699 700
  , CN.Assign 700 "stack_pointer" (Num 0)
  , CN.Seq 700 701
  , CN.Assign 701 "stack_pointer" (Num 0)
  , CN.Seq 701 702
  , CN.Assign 702 "res" (Num 0)
  , CN.Seq 702 703
  , CN.Assign 703 "undefed" (Num 0)
  , CN.Seq 703 704
  , CN.Assign 704 "stack_pointer" (Num 0)
  , CN.Seq 704 705
  , CN.Var 705 "word"
  , CN.Seq 705 706
  , CN.Assign 706 "opcode" (Num 0)
  , CN.Seq 706 707
  , CN.Assign 707 "oparg" (Num 0)
  , CN.Seq 707 708
  , CN.Branch 708 (Eq (Num 0) (Num 1)) 709 712
  , CN.Var 709 "word"
  , CN.Seq 709 710
  , CN.Assign 710 "opcode" (Num 0)
  , CN.Seq 710 711
  , CN.Assign 711 "oparg" (Num 0)
  , CN.Seq 711 712
  , CN.Seq 711 708
  , CN.Var 712 "LOOP_FOOTER"
  , CN.Seq 712 713
  , CN.Seq 712 35
  , CN.Branch 713 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 715 765
  , CN.Var 715 "NOP_715"
  , CN.Var 716 "__CLABEL_TARGET_BINARY_OP_SUBSCR_TUPLE_INT"
  , CN.Seq 716 717
  , CN.Var 717 "this_instr"
  , CN.Seq 717 718
  , CN.Assign 718 "undefed" (Num 0)
  , CN.Seq 718 719
  , CN.Assign 719 "next_instr" (Num 0)
  , CN.Seq 719 720
  , CN.Var 720 "value"
  , CN.Seq 720 721
  , CN.Var 721 "nos"
  , CN.Seq 721 722
  , CN.Var 722 "tuple_st"
  , CN.Seq 722 723
  , CN.Var 723 "sub_st"
  , CN.Seq 723 724
  , CN.Var 724 "res"
  , CN.Seq 724 725
  , CN.Assign 725 "value" (Num 0)
  , CN.Seq 725 726
  , CN.Var 726 "value_o"
  , CN.Seq 726 727
  , CN.Branch 727 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 729 729
  , CN.Seq 728 42
  , CN.Seq 728 730
  , CN.Var 729 "NOP_729"
  , CN.Seq 729 730
  , CN.Var 730 "IF_ELSE_FOOTER"
  , CN.Assign 731 "nos" (Num 0)
  , CN.Seq 731 732
  , CN.Var 732 "o"
  , CN.Seq 732 733
  , CN.Branch 733 (Eq (Plus (Plus (Num 0) (Plus (Id "PyTuple_Type") (Num 0))) (Num 0)) (Num 1)) 735 735
  , CN.Seq 734 42
  , CN.Seq 734 736
  , CN.Var 735 "NOP_735"
  , CN.Seq 735 736
  , CN.Var 736 "IF_ELSE_FOOTER"
  , CN.Assign 737 "sub_st" (Num 0)
  , CN.Seq 737 738
  , CN.Assign 738 "tuple_st" (Num 0)
  , CN.Seq 738 739
  , CN.Var 739 "sub"
  , CN.Seq 739 740
  , CN.Var 740 "tuple"
  , CN.Seq 740 741
  , CN.Branch 741 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 743 743
  , CN.Seq 742 42
  , CN.Seq 742 744
  , CN.Var 743 "NOP_743"
  , CN.Seq 743 744
  , CN.Var 744 "IF_ELSE_FOOTER"
  , CN.Var 745 "index"
  , CN.Seq 745 746
  , CN.Branch 746 (Eq (Plus (Id "index") (Num 0)) (Num 1)) 748 748
  , CN.Seq 747 42
  , CN.Seq 747 749
  , CN.Var 748 "NOP_748"
  , CN.Seq 748 749
  , CN.Var 749 "IF_ELSE_FOOTER"
  , CN.Var 750 "res_o"
  , CN.Seq 750 751
  , CN.Assign 751 "res" (Num 0)
  , CN.Seq 751 752
  , CN.Assign 752 "stack_pointer" (Num 0)
  , CN.Seq 752 753
  , CN.Var 753 "tmp"
  , CN.Seq 753 754
  , CN.Assign 754 "tuple_st" (Num 0)
  , CN.Seq 754 755
  , CN.Assign 755 "undefed" (Num 0)
  , CN.Seq 755 756
  , CN.Assign 756 "stack_pointer" (Num 0)
  , CN.Seq 756 757
  , CN.Var 757 "word"
  , CN.Seq 757 758
  , CN.Assign 758 "opcode" (Num 0)
  , CN.Seq 758 759
  , CN.Assign 759 "oparg" (Num 0)
  , CN.Seq 759 760
  , CN.Branch 760 (Eq (Num 0) (Num 1)) 761 764
  , CN.Var 761 "word"
  , CN.Seq 761 762
  , CN.Assign 762 "opcode" (Num 0)
  , CN.Seq 762 763
  , CN.Assign 763 "oparg" (Num 0)
  , CN.Seq 763 764
  , CN.Seq 763 760
  , CN.Var 764 "LOOP_FOOTER"
  , CN.Seq 764 765
  , CN.Seq 764 35
  , CN.Branch 765 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 767 807
  , CN.Var 767 "NOP_767"
  , CN.Var 768 "__CLABEL_TARGET_BINARY_OP_SUBTRACT_FLOAT"
  , CN.Seq 768 769
  , CN.Var 769 "this_instr"
  , CN.Seq 769 770
  , CN.Assign 770 "undefed" (Num 0)
  , CN.Seq 770 771
  , CN.Assign 771 "next_instr" (Num 0)
  , CN.Seq 771 772
  , CN.Var 772 "value"
  , CN.Seq 772 773
  , CN.Var 773 "left"
  , CN.Seq 773 774
  , CN.Var 774 "right"
  , CN.Seq 774 775
  , CN.Var 775 "res"
  , CN.Seq 775 776
  , CN.Assign 776 "value" (Num 0)
  , CN.Seq 776 777
  , CN.Var 777 "value_o"
  , CN.Seq 777 778
  , CN.Branch 778 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 780 780
  , CN.Seq 779 42
  , CN.Seq 779 781
  , CN.Var 780 "NOP_780"
  , CN.Seq 780 781
  , CN.Var 781 "IF_ELSE_FOOTER"
  , CN.Assign 782 "left" (Num 0)
  , CN.Seq 782 783
  , CN.Var 783 "left_o"
  , CN.Seq 783 784
  , CN.Branch 784 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFloat_Type") (Num 0))) (Num 0)) (Num 1)) 786 786
  , CN.Seq 785 42
  , CN.Seq 785 787
  , CN.Var 786 "NOP_786"
  , CN.Seq 786 787
  , CN.Var 787 "IF_ELSE_FOOTER"
  , CN.Assign 788 "right" (Num 0)
  , CN.Seq 788 789
  , CN.Var 789 "left_o"
  , CN.Seq 789 790
  , CN.Var 790 "right_o"
  , CN.Seq 790 791
  , CN.Var 791 "dres"
  , CN.Seq 791 792
  , CN.Assign 792 "res" (Num 0)
  , CN.Seq 792 793
  , CN.Branch 793 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 795 795
  , CN.Seq 794 1813
  , CN.Seq 794 796
  , CN.Var 795 "NOP_795"
  , CN.Seq 795 796
  , CN.Var 796 "IF_ELSE_FOOTER"
  , CN.Assign 797 "undefed" (Num 0)
  , CN.Seq 797 798
  , CN.Assign 798 "stack_pointer" (Num 0)
  , CN.Seq 798 799
  , CN.Var 799 "word"
  , CN.Seq 799 800
  , CN.Assign 800 "opcode" (Num 0)
  , CN.Seq 800 801
  , CN.Assign 801 "oparg" (Num 0)
  , CN.Seq 801 802
  , CN.Branch 802 (Eq (Num 0) (Num 1)) 803 806
  , CN.Var 803 "word"
  , CN.Seq 803 804
  , CN.Assign 804 "opcode" (Num 0)
  , CN.Seq 804 805
  , CN.Assign 805 "oparg" (Num 0)
  , CN.Seq 805 806
  , CN.Seq 805 802
  , CN.Var 806 "LOOP_FOOTER"
  , CN.Seq 806 807
  , CN.Seq 806 35
  , CN.Branch 807 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 809 848
  , CN.Var 809 "NOP_809"
  , CN.Var 810 "__CLABEL_TARGET_BINARY_OP_SUBTRACT_INT"
  , CN.Seq 810 811
  , CN.Var 811 "this_instr"
  , CN.Seq 811 812
  , CN.Assign 812 "undefed" (Num 0)
  , CN.Seq 812 813
  , CN.Assign 813 "next_instr" (Num 0)
  , CN.Seq 813 814
  , CN.Var 814 "value"
  , CN.Seq 814 815
  , CN.Var 815 "left"
  , CN.Seq 815 816
  , CN.Var 816 "right"
  , CN.Seq 816 817
  , CN.Var 817 "res"
  , CN.Seq 817 818
  , CN.Assign 818 "value" (Num 0)
  , CN.Seq 818 819
  , CN.Var 819 "value_o"
  , CN.Seq 819 820
  , CN.Branch 820 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 822 822
  , CN.Seq 821 42
  , CN.Seq 821 823
  , CN.Var 822 "NOP_822"
  , CN.Seq 822 823
  , CN.Var 823 "IF_ELSE_FOOTER"
  , CN.Assign 824 "left" (Num 0)
  , CN.Seq 824 825
  , CN.Var 825 "left_o"
  , CN.Seq 825 826
  , CN.Branch 826 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 828 828
  , CN.Seq 827 42
  , CN.Seq 827 829
  , CN.Var 828 "NOP_828"
  , CN.Seq 828 829
  , CN.Var 829 "IF_ELSE_FOOTER"
  , CN.Assign 830 "right" (Num 0)
  , CN.Seq 830 831
  , CN.Var 831 "left_o"
  , CN.Seq 831 832
  , CN.Var 832 "right_o"
  , CN.Seq 832 833
  , CN.Assign 833 "res" (Num 0)
  , CN.Seq 833 834
  , CN.Branch 834 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 836 836
  , CN.Seq 835 42
  , CN.Seq 835 837
  , CN.Var 836 "NOP_836"
  , CN.Seq 836 837
  , CN.Var 837 "IF_ELSE_FOOTER"
  , CN.Assign 838 "undefed" (Num 0)
  , CN.Seq 838 839
  , CN.Assign 839 "stack_pointer" (Num 0)
  , CN.Seq 839 840
  , CN.Var 840 "word"
  , CN.Seq 840 841
  , CN.Assign 841 "opcode" (Num 0)
  , CN.Seq 841 842
  , CN.Assign 842 "oparg" (Num 0)
  , CN.Seq 842 843
  , CN.Branch 843 (Eq (Num 0) (Num 1)) 844 847
  , CN.Var 844 "word"
  , CN.Seq 844 845
  , CN.Assign 845 "opcode" (Num 0)
  , CN.Seq 845 846
  , CN.Assign 846 "oparg" (Num 0)
  , CN.Seq 846 847
  , CN.Seq 846 843
  , CN.Var 847 "LOOP_FOOTER"
  , CN.Seq 847 848
  , CN.Seq 847 35
  , CN.Branch 848 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 850 931
  , CN.Var 850 "NOP_850"
  , CN.Var 851 "__CLABEL_TARGET_BINARY_SLICE"
  , CN.Seq 851 852
  , CN.Assign 852 "undefed" (Num 0)
  , CN.Seq 852 853
  , CN.Assign 853 "next_instr" (Num 0)
  , CN.Seq 853 854
  , CN.Var 854 "container"
  , CN.Seq 854 855
  , CN.Var 855 "start"
  , CN.Seq 855 856
  , CN.Var 856 "stop"
  , CN.Seq 856 857
  , CN.Var 857 "res"
  , CN.Seq 857 858
  , CN.Assign 858 "stop" (Num 0)
  , CN.Seq 858 859
  , CN.Assign 859 "start" (Num 0)
  , CN.Seq 859 860
  , CN.Assign 860 "container" (Num 0)
  , CN.Seq 860 861
  , CN.Var 861 "slice"
  , CN.Seq 861 862
  , CN.Assign 862 "stack_pointer" (Num 0)
  , CN.Seq 862 863
  , CN.Var 863 "res_o"
  , CN.Seq 863 864
  , CN.Branch 864 (Eq (Plus (Id "slice") (Num 0)) (Num 1)) 866 867
  , CN.Assign 866 "res_o" (Num 0)
  , CN.Seq 866 913
  , CN.Assign 867 "stack_pointer" (Num 0)
  , CN.Seq 867 868
  , CN.Assign 868 "res_o" (Num 0)
  , CN.Seq 868 869
  , CN.Var 869 "op"
  , CN.Seq 869 870
  , CN.Branch 870 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 872 887
  , CN.Var 872 "tracer"
  , CN.Seq 872 873
  , CN.Branch 873 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 875 876
  , CN.Var 875 "data"
  , CN.Seq 875 876
  , CN.Seq 875 877
  , CN.Var 876 "NOP_876"
  , CN.Seq 876 877
  , CN.Var 877 "IF_ELSE_FOOTER"
  , CN.Branch 878 (Eq (Num 0) (Num 1)) 879 885
  , CN.Var 879 "tracer"
  , CN.Seq 879 880
  , CN.Branch 880 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 882 883
  , CN.Var 882 "data"
  , CN.Seq 882 883
  , CN.Seq 882 884
  , CN.Var 883 "NOP_883"
  , CN.Seq 883 884
  , CN.Var 884 "IF_ELSE_FOOTER"
  , CN.Seq 884 878
  , CN.Var 885 "LOOP_FOOTER"
  , CN.Seq 885 886
  , CN.Var 886 "dealloc"
  , CN.Seq 886 887
  , CN.Seq 886 888
  , CN.Var 887 "NOP_887"
  , CN.Seq 887 888
  , CN.Var 888 "IF_ELSE_FOOTER"
  , CN.Branch 889 (Eq (Num 0) (Num 1)) 890 910
  , CN.Var 890 "op"
  , CN.Seq 890 891
  , CN.Branch 891 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 893 908
  , CN.Var 893 "tracer"
  , CN.Seq 893 894
  , CN.Branch 894 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 896 897
  , CN.Var 896 "data"
  , CN.Seq 896 897
  , CN.Seq 896 898
  , CN.Var 897 "NOP_897"
  , CN.Seq 897 898
  , CN.Var 898 "IF_ELSE_FOOTER"
  , CN.Branch 899 (Eq (Num 0) (Num 1)) 900 906
  , CN.Var 900 "tracer"
  , CN.Seq 900 901
  , CN.Branch 901 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 903 904
  , CN.Var 903 "data"
  , CN.Seq 903 904
  , CN.Seq 903 905
  , CN.Var 904 "NOP_904"
  , CN.Seq 904 905
  , CN.Var 905 "IF_ELSE_FOOTER"
  , CN.Seq 905 899
  , CN.Var 906 "LOOP_FOOTER"
  , CN.Seq 906 907
  , CN.Var 907 "dealloc"
  , CN.Seq 907 908
  , CN.Seq 907 909
  , CN.Var 908 "NOP_908"
  , CN.Seq 908 909
  , CN.Var 909 "IF_ELSE_FOOTER"
  , CN.Seq 909 889
  , CN.Var 910 "LOOP_FOOTER"
  , CN.Seq 910 911
  , CN.Assign 911 "stack_pointer" (Num 0)
  , CN.Seq 911 912
  , CN.Assign 912 "stack_pointer" (Num 0)
  , CN.Seq 912 913
  , CN.Var 913 "IF_ELSE_FOOTER"
  , CN.Assign 914 "stack_pointer" (Num 0)
  , CN.Seq 914 915
  , CN.Assign 915 "stack_pointer" (Num 0)
  , CN.Seq 915 916
  , CN.Branch 916 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 918 918
  , CN.Seq 917 1817
  , CN.Seq 917 919
  , CN.Var 918 "NOP_918"
  , CN.Seq 918 919
  , CN.Var 919 "IF_ELSE_FOOTER"
  , CN.Assign 920 "res" (Num 0)
  , CN.Seq 920 921
  , CN.Assign 921 "undefed" (Num 0)
  , CN.Seq 921 922
  , CN.Assign 922 "stack_pointer" (Num 0)
  , CN.Seq 922 923
  , CN.Var 923 "word"
  , CN.Seq 923 924
  , CN.Assign 924 "opcode" (Num 0)
  , CN.Seq 924 925
  , CN.Assign 925 "oparg" (Num 0)
  , CN.Seq 925 926
  , CN.Branch 926 (Eq (Num 0) (Num 1)) 927 930
  , CN.Var 927 "word"
  , CN.Seq 927 928
  , CN.Assign 928 "opcode" (Num 0)
  , CN.Seq 928 929
  , CN.Assign 929 "oparg" (Num 0)
  , CN.Seq 929 930
  , CN.Seq 929 926
  , CN.Var 930 "LOOP_FOOTER"
  , CN.Seq 930 931
  , CN.Seq 930 35
  , CN.Branch 931 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 933 980
  , CN.Var 933 "NOP_933"
  , CN.Var 934 "__CLABEL_TARGET_BUILD_INTERPOLATION"
  , CN.Seq 934 935
  , CN.Assign 935 "undefed" (Num 0)
  , CN.Seq 935 936
  , CN.Assign 936 "next_instr" (Num 0)
  , CN.Seq 936 937
  , CN.Var 937 "value"
  , CN.Seq 937 938
  , CN.Var 938 "str"
  , CN.Seq 938 939
  , CN.Var 939 "format"
  , CN.Seq 939 940
  , CN.Var 940 "interpolation"
  , CN.Seq 940 941
  , CN.Assign 941 "format" (Num 0)
  , CN.Seq 941 942
  , CN.Assign 942 "str" (Num 0)
  , CN.Seq 942 943
  , CN.Assign 943 "value" (Num 0)
  , CN.Seq 943 944
  , CN.Var 944 "value_o"
  , CN.Seq 944 945
  , CN.Var 945 "str_o"
  , CN.Seq 945 946
  , CN.Var 946 "conversion"
  , CN.Seq 946 947
  , CN.Var 947 "format_o"
  , CN.Seq 947 948
  , CN.Branch 948 (Eq (Plus (Id "oparg") (Num 0)) (Num 1)) 950 951
  , CN.Assign 950 "format_o" (Num 0)
  , CN.Seq 950 952
  , CN.Assign 951 "format_o" (Num 0)
  , CN.Seq 951 952
  , CN.Var 952 "IF_ELSE_FOOTER"
  , CN.Var 953 "interpolation_o"
  , CN.Seq 953 954
  , CN.Assign 954 "stack_pointer" (Num 0)
  , CN.Seq 954 955
  , CN.Branch 955 (Eq (Plus (Id "oparg") (Num 0)) (Num 1)) 957 959
  , CN.Assign 957 "stack_pointer" (Num 0)
  , CN.Seq 957 958
  , CN.Assign 958 "stack_pointer" (Num 0)
  , CN.Seq 958 959
  , CN.Seq 958 960
  , CN.Assign 959 "stack_pointer" (Num 0)
  , CN.Seq 959 960
  , CN.Var 960 "IF_ELSE_FOOTER"
  , CN.Assign 961 "stack_pointer" (Num 0)
  , CN.Seq 961 962
  , CN.Assign 962 "stack_pointer" (Num 0)
  , CN.Seq 962 963
  , CN.Assign 963 "stack_pointer" (Num 0)
  , CN.Seq 963 964
  , CN.Assign 964 "stack_pointer" (Num 0)
  , CN.Seq 964 965
  , CN.Branch 965 (Eq (Plus (Id "interpolation_o") (Num 0)) (Num 1)) 967 967
  , CN.Seq 966 1817
  , CN.Seq 966 968
  , CN.Var 967 "NOP_967"
  , CN.Seq 967 968
  , CN.Var 968 "IF_ELSE_FOOTER"
  , CN.Assign 969 "interpolation" (Num 0)
  , CN.Seq 969 970
  , CN.Assign 970 "undefed" (Num 0)
  , CN.Seq 970 971
  , CN.Assign 971 "stack_pointer" (Num 0)
  , CN.Seq 971 972
  , CN.Var 972 "word"
  , CN.Seq 972 973
  , CN.Assign 973 "opcode" (Num 0)
  , CN.Seq 973 974
  , CN.Assign 974 "oparg" (Num 0)
  , CN.Seq 974 975
  , CN.Branch 975 (Eq (Num 0) (Num 1)) 976 979
  , CN.Var 976 "word"
  , CN.Seq 976 977
  , CN.Assign 977 "opcode" (Num 0)
  , CN.Seq 977 978
  , CN.Assign 978 "oparg" (Num 0)
  , CN.Seq 978 979
  , CN.Seq 978 975
  , CN.Var 979 "LOOP_FOOTER"
  , CN.Seq 979 980
  , CN.Seq 979 35
  , CN.Branch 980 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 982 1006
  , CN.Var 982 "NOP_982"
  , CN.Var 983 "__CLABEL_TARGET_BUILD_LIST"
  , CN.Seq 983 984
  , CN.Assign 984 "undefed" (Num 0)
  , CN.Seq 984 985
  , CN.Assign 985 "next_instr" (Num 0)
  , CN.Seq 985 986
  , CN.Var 986 "values"
  , CN.Seq 986 987
  , CN.Var 987 "list"
  , CN.Seq 987 988
  , CN.Assign 988 "values" (Num 0)
  , CN.Seq 988 989
  , CN.Var 989 "list_o"
  , CN.Seq 989 990
  , CN.Assign 990 "stack_pointer" (Num 0)
  , CN.Seq 990 991
  , CN.Branch 991 (Eq (Plus (Id "list_o") (Num 0)) (Num 1)) 993 993
  , CN.Seq 992 1817
  , CN.Seq 992 994
  , CN.Var 993 "NOP_993"
  , CN.Seq 993 994
  , CN.Var 994 "IF_ELSE_FOOTER"
  , CN.Assign 995 "list" (Num 0)
  , CN.Seq 995 996
  , CN.Assign 996 "undefed" (Num 0)
  , CN.Seq 996 997
  , CN.Assign 997 "stack_pointer" (Num 0)
  , CN.Seq 997 998
  , CN.Var 998 "word"
  , CN.Seq 998 999
  , CN.Assign 999 "opcode" (Num 0)
  , CN.Seq 999 1000
  , CN.Assign 1000 "oparg" (Num 0)
  , CN.Seq 1000 1001
  , CN.Branch 1001 (Eq (Num 0) (Num 1)) 1002 1005
  , CN.Var 1002 "word"
  , CN.Seq 1002 1003
  , CN.Assign 1003 "opcode" (Num 0)
  , CN.Seq 1003 1004
  , CN.Assign 1004 "oparg" (Num 0)
  , CN.Seq 1004 1005
  , CN.Seq 1004 1001
  , CN.Var 1005 "LOOP_FOOTER"
  , CN.Seq 1005 1006
  , CN.Seq 1005 35
  , CN.Branch 1006 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1008 1054
  , CN.Var 1008 "NOP_1008"
  , CN.Var 1009 "__CLABEL_TARGET_BUILD_MAP"
  , CN.Seq 1009 1010
  , CN.Assign 1010 "undefed" (Num 0)
  , CN.Seq 1010 1011
  , CN.Assign 1011 "next_instr" (Num 0)
  , CN.Seq 1011 1012
  , CN.Var 1012 "values"
  , CN.Seq 1012 1013
  , CN.Var 1013 "map"
  , CN.Seq 1013 1014
  , CN.Assign 1014 "values" (Num 0)
  , CN.Seq 1014 1015
  , CN.Var 1015 "values_o_temp"
  , CN.Seq 1015 1016
  , CN.Var 1016 "values_o"
  , CN.Seq 1016 1017
  , CN.Branch 1017 (Eq (Plus (Id "values_o") (Num 0)) (Num 1)) 1019 1027
  , CN.Var 1019 "tmp"
  , CN.Seq 1019 1020
  , CN.Var 1020 "_i"
  , CN.Seq 1020 1021
  , CN.Branch 1021 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1022 1024
  , CN.Assign 1022 "tmp" (Num 0)
  , CN.Seq 1022 1023
  , CN.Assign 1023 "undefed" (Num 0)
  , CN.Seq 1023 1024
  , CN.Seq 1023 1021
  , CN.Var 1024 "LOOP_FOOTER"
  , CN.Seq 1024 1025
  , CN.Assign 1025 "stack_pointer" (Num 0)
  , CN.Seq 1025 1026
  , CN.Assign 1026 "stack_pointer" (Num 0)
  , CN.Seq 1026 1027
  , CN.Seq 1026 1817
  , CN.Seq 1026 1028
  , CN.Var 1027 "NOP_1027"
  , CN.Seq 1027 1028
  , CN.Var 1028 "IF_ELSE_FOOTER"
  , CN.Var 1029 "map_o"
  , CN.Seq 1029 1030
  , CN.Assign 1030 "stack_pointer" (Num 0)
  , CN.Seq 1030 1031
  , CN.Var 1031 "tmp"
  , CN.Seq 1031 1032
  , CN.Var 1032 "_i"
  , CN.Seq 1032 1033
  , CN.Branch 1033 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1034 1036
  , CN.Assign 1034 "tmp" (Num 0)
  , CN.Seq 1034 1035
  , CN.Assign 1035 "undefed" (Num 0)
  , CN.Seq 1035 1036
  , CN.Seq 1035 1033
  , CN.Var 1036 "LOOP_FOOTER"
  , CN.Seq 1036 1037
  , CN.Assign 1037 "stack_pointer" (Num 0)
  , CN.Seq 1037 1038
  , CN.Assign 1038 "stack_pointer" (Num 0)
  , CN.Seq 1038 1039
  , CN.Branch 1039 (Eq (Plus (Id "map_o") (Num 0)) (Num 1)) 1041 1041
  , CN.Seq 1040 1817
  , CN.Seq 1040 1042
  , CN.Var 1041 "NOP_1041"
  , CN.Seq 1041 1042
  , CN.Var 1042 "IF_ELSE_FOOTER"
  , CN.Assign 1043 "map" (Num 0)
  , CN.Seq 1043 1044
  , CN.Assign 1044 "undefed" (Num 0)
  , CN.Seq 1044 1045
  , CN.Assign 1045 "stack_pointer" (Num 0)
  , CN.Seq 1045 1046
  , CN.Var 1046 "word"
  , CN.Seq 1046 1047
  , CN.Assign 1047 "opcode" (Num 0)
  , CN.Seq 1047 1048
  , CN.Assign 1048 "oparg" (Num 0)
  , CN.Seq 1048 1049
  , CN.Branch 1049 (Eq (Num 0) (Num 1)) 1050 1053
  , CN.Var 1050 "word"
  , CN.Seq 1050 1051
  , CN.Assign 1051 "opcode" (Num 0)
  , CN.Seq 1051 1052
  , CN.Assign 1052 "oparg" (Num 0)
  , CN.Seq 1052 1053
  , CN.Seq 1052 1049
  , CN.Var 1053 "LOOP_FOOTER"
  , CN.Seq 1053 1054
  , CN.Seq 1053 35
  , CN.Branch 1054 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1056 1148
  , CN.Var 1056 "NOP_1056"
  , CN.Var 1057 "__CLABEL_TARGET_BUILD_SET"
  , CN.Seq 1057 1058
  , CN.Assign 1058 "undefed" (Num 0)
  , CN.Seq 1058 1059
  , CN.Assign 1059 "next_instr" (Num 0)
  , CN.Seq 1059 1060
  , CN.Var 1060 "values"
  , CN.Seq 1060 1061
  , CN.Var 1061 "set"
  , CN.Seq 1061 1062
  , CN.Assign 1062 "values" (Num 0)
  , CN.Seq 1062 1063
  , CN.Var 1063 "set_o"
  , CN.Seq 1063 1064
  , CN.Assign 1064 "stack_pointer" (Num 0)
  , CN.Seq 1064 1065
  , CN.Branch 1065 (Eq (Plus (Id "set_o") (Num 0)) (Num 1)) 1067 1075
  , CN.Var 1067 "tmp"
  , CN.Seq 1067 1068
  , CN.Var 1068 "_i"
  , CN.Seq 1068 1069
  , CN.Branch 1069 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1070 1072
  , CN.Assign 1070 "tmp" (Num 0)
  , CN.Seq 1070 1071
  , CN.Assign 1071 "undefed" (Num 0)
  , CN.Seq 1071 1072
  , CN.Seq 1071 1069
  , CN.Var 1072 "LOOP_FOOTER"
  , CN.Seq 1072 1073
  , CN.Assign 1073 "stack_pointer" (Num 0)
  , CN.Seq 1073 1074
  , CN.Assign 1074 "stack_pointer" (Num 0)
  , CN.Seq 1074 1075
  , CN.Seq 1074 1817
  , CN.Seq 1074 1076
  , CN.Var 1075 "NOP_1075"
  , CN.Seq 1075 1076
  , CN.Var 1076 "IF_ELSE_FOOTER"
  , CN.Var 1077 "err"
  , CN.Seq 1077 1078
  , CN.Var 1078 "i"
  , CN.Seq 1078 1079
  , CN.Branch 1079 (Eq (Plus (Id "i") (Id "oparg")) (Num 1)) 1080 1088
  , CN.Var 1080 "value"
  , CN.Seq 1080 1081
  , CN.Assign 1081 "undefed" (Num 0)
  , CN.Seq 1081 1082
  , CN.Branch 1082 (Eq (Plus (Id "err") (Num 0)) (Num 1)) 1084 1086
  , CN.Assign 1084 "err" (Num 0)
  , CN.Seq 1084 1085
  , CN.Assign 1085 "stack_pointer" (Num 0)
  , CN.Seq 1085 1086
  , CN.Seq 1085 1087
  , CN.Assign 1086 "stack_pointer" (Num 0)
  , CN.Seq 1086 1087
  , CN.Var 1087 "IF_ELSE_FOOTER"
  , CN.Seq 1087 1079
  , CN.Var 1088 "LOOP_FOOTER"
  , CN.Seq 1088 1089
  , CN.Branch 1089 (Eq (Id "err") (Num 1)) 1091 1135
  , CN.Assign 1091 "stack_pointer" (Num 0)
  , CN.Seq 1091 1092
  , CN.Var 1092 "op"
  , CN.Seq 1092 1093
  , CN.Branch 1093 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1095 1110
  , CN.Var 1095 "tracer"
  , CN.Seq 1095 1096
  , CN.Branch 1096 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1098 1099
  , CN.Var 1098 "data"
  , CN.Seq 1098 1099
  , CN.Seq 1098 1100
  , CN.Var 1099 "NOP_1099"
  , CN.Seq 1099 1100
  , CN.Var 1100 "IF_ELSE_FOOTER"
  , CN.Branch 1101 (Eq (Num 0) (Num 1)) 1102 1108
  , CN.Var 1102 "tracer"
  , CN.Seq 1102 1103
  , CN.Branch 1103 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1105 1106
  , CN.Var 1105 "data"
  , CN.Seq 1105 1106
  , CN.Seq 1105 1107
  , CN.Var 1106 "NOP_1106"
  , CN.Seq 1106 1107
  , CN.Var 1107 "IF_ELSE_FOOTER"
  , CN.Seq 1107 1101
  , CN.Var 1108 "LOOP_FOOTER"
  , CN.Seq 1108 1109
  , CN.Var 1109 "dealloc"
  , CN.Seq 1109 1110
  , CN.Seq 1109 1111
  , CN.Var 1110 "NOP_1110"
  , CN.Seq 1110 1111
  , CN.Var 1111 "IF_ELSE_FOOTER"
  , CN.Branch 1112 (Eq (Num 0) (Num 1)) 1113 1133
  , CN.Var 1113 "op"
  , CN.Seq 1113 1114
  , CN.Branch 1114 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1116 1131
  , CN.Var 1116 "tracer"
  , CN.Seq 1116 1117
  , CN.Branch 1117 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1119 1120
  , CN.Var 1119 "data"
  , CN.Seq 1119 1120
  , CN.Seq 1119 1121
  , CN.Var 1120 "NOP_1120"
  , CN.Seq 1120 1121
  , CN.Var 1121 "IF_ELSE_FOOTER"
  , CN.Branch 1122 (Eq (Num 0) (Num 1)) 1123 1129
  , CN.Var 1123 "tracer"
  , CN.Seq 1123 1124
  , CN.Branch 1124 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1126 1127
  , CN.Var 1126 "data"
  , CN.Seq 1126 1127
  , CN.Seq 1126 1128
  , CN.Var 1127 "NOP_1127"
  , CN.Seq 1127 1128
  , CN.Var 1128 "IF_ELSE_FOOTER"
  , CN.Seq 1128 1122
  , CN.Var 1129 "LOOP_FOOTER"
  , CN.Seq 1129 1130
  , CN.Var 1130 "dealloc"
  , CN.Seq 1130 1131
  , CN.Seq 1130 1132
  , CN.Var 1131 "NOP_1131"
  , CN.Seq 1131 1132
  , CN.Var 1132 "IF_ELSE_FOOTER"
  , CN.Seq 1132 1112
  , CN.Var 1133 "LOOP_FOOTER"
  , CN.Seq 1133 1134
  , CN.Assign 1134 "stack_pointer" (Num 0)
  , CN.Seq 1134 1135
  , CN.Seq 1134 1817
  , CN.Seq 1134 1136
  , CN.Var 1135 "NOP_1135"
  , CN.Seq 1135 1136
  , CN.Var 1136 "IF_ELSE_FOOTER"
  , CN.Assign 1137 "set" (Num 0)
  , CN.Seq 1137 1138
  , CN.Assign 1138 "undefed" (Num 0)
  , CN.Seq 1138 1139
  , CN.Assign 1139 "stack_pointer" (Num 0)
  , CN.Seq 1139 1140
  , CN.Var 1140 "word"
  , CN.Seq 1140 1141
  , CN.Assign 1141 "opcode" (Num 0)
  , CN.Seq 1141 1142
  , CN.Assign 1142 "oparg" (Num 0)
  , CN.Seq 1142 1143
  , CN.Branch 1143 (Eq (Num 0) (Num 1)) 1144 1147
  , CN.Var 1144 "word"
  , CN.Seq 1144 1145
  , CN.Assign 1145 "opcode" (Num 0)
  , CN.Seq 1145 1146
  , CN.Assign 1146 "oparg" (Num 0)
  , CN.Seq 1146 1147
  , CN.Seq 1146 1143
  , CN.Var 1147 "LOOP_FOOTER"
  , CN.Seq 1147 1148
  , CN.Seq 1147 35
  , CN.Branch 1148 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1150 1184
  , CN.Var 1150 "NOP_1150"
  , CN.Var 1151 "__CLABEL_TARGET_BUILD_SLICE"
  , CN.Seq 1151 1152
  , CN.Assign 1152 "undefed" (Num 0)
  , CN.Seq 1152 1153
  , CN.Assign 1153 "next_instr" (Num 0)
  , CN.Seq 1153 1154
  , CN.Var 1154 "args"
  , CN.Seq 1154 1155
  , CN.Var 1155 "slice"
  , CN.Seq 1155 1156
  , CN.Assign 1156 "args" (Num 0)
  , CN.Seq 1156 1157
  , CN.Var 1157 "start_o"
  , CN.Seq 1157 1158
  , CN.Var 1158 "stop_o"
  , CN.Seq 1158 1159
  , CN.Var 1159 "step_o"
  , CN.Seq 1159 1160
  , CN.Var 1160 "slice_o"
  , CN.Seq 1160 1161
  , CN.Var 1161 "tmp"
  , CN.Seq 1161 1162
  , CN.Var 1162 "_i"
  , CN.Seq 1162 1163
  , CN.Branch 1163 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1164 1166
  , CN.Assign 1164 "tmp" (Num 0)
  , CN.Seq 1164 1165
  , CN.Assign 1165 "undefed" (Num 0)
  , CN.Seq 1165 1166
  , CN.Seq 1165 1163
  , CN.Var 1166 "LOOP_FOOTER"
  , CN.Seq 1166 1167
  , CN.Assign 1167 "stack_pointer" (Num 0)
  , CN.Seq 1167 1168
  , CN.Assign 1168 "stack_pointer" (Num 0)
  , CN.Seq 1168 1169
  , CN.Branch 1169 (Eq (Plus (Id "slice_o") (Num 0)) (Num 1)) 1171 1171
  , CN.Seq 1170 1817
  , CN.Seq 1170 1172
  , CN.Var 1171 "NOP_1171"
  , CN.Seq 1171 1172
  , CN.Var 1172 "IF_ELSE_FOOTER"
  , CN.Assign 1173 "slice" (Num 0)
  , CN.Seq 1173 1174
  , CN.Assign 1174 "undefed" (Num 0)
  , CN.Seq 1174 1175
  , CN.Assign 1175 "stack_pointer" (Num 0)
  , CN.Seq 1175 1176
  , CN.Var 1176 "word"
  , CN.Seq 1176 1177
  , CN.Assign 1177 "opcode" (Num 0)
  , CN.Seq 1177 1178
  , CN.Assign 1178 "oparg" (Num 0)
  , CN.Seq 1178 1179
  , CN.Branch 1179 (Eq (Num 0) (Num 1)) 1180 1183
  , CN.Var 1180 "word"
  , CN.Seq 1180 1181
  , CN.Assign 1181 "opcode" (Num 0)
  , CN.Seq 1181 1182
  , CN.Assign 1182 "oparg" (Num 0)
  , CN.Seq 1182 1183
  , CN.Seq 1182 1179
  , CN.Var 1183 "LOOP_FOOTER"
  , CN.Seq 1183 1184
  , CN.Seq 1183 35
  , CN.Branch 1184 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1186 1231
  , CN.Var 1186 "NOP_1186"
  , CN.Var 1187 "__CLABEL_TARGET_BUILD_STRING"
  , CN.Seq 1187 1188
  , CN.Assign 1188 "undefed" (Num 0)
  , CN.Seq 1188 1189
  , CN.Assign 1189 "next_instr" (Num 0)
  , CN.Seq 1189 1190
  , CN.Var 1190 "pieces"
  , CN.Seq 1190 1191
  , CN.Var 1191 "str"
  , CN.Seq 1191 1192
  , CN.Assign 1192 "pieces" (Num 0)
  , CN.Seq 1192 1193
  , CN.Var 1193 "pieces_o_temp"
  , CN.Seq 1193 1194
  , CN.Var 1194 "pieces_o"
  , CN.Seq 1194 1195
  , CN.Branch 1195 (Eq (Plus (Id "pieces_o") (Num 0)) (Num 1)) 1197 1205
  , CN.Var 1197 "tmp"
  , CN.Seq 1197 1198
  , CN.Var 1198 "_i"
  , CN.Seq 1198 1199
  , CN.Branch 1199 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1200 1202
  , CN.Assign 1200 "tmp" (Num 0)
  , CN.Seq 1200 1201
  , CN.Assign 1201 "undefed" (Num 0)
  , CN.Seq 1201 1202
  , CN.Seq 1201 1199
  , CN.Var 1202 "LOOP_FOOTER"
  , CN.Seq 1202 1203
  , CN.Assign 1203 "stack_pointer" (Num 0)
  , CN.Seq 1203 1204
  , CN.Assign 1204 "stack_pointer" (Num 0)
  , CN.Seq 1204 1205
  , CN.Seq 1204 1817
  , CN.Seq 1204 1206
  , CN.Var 1205 "NOP_1205"
  , CN.Seq 1205 1206
  , CN.Var 1206 "IF_ELSE_FOOTER"
  , CN.Var 1207 "str_o"
  , CN.Seq 1207 1208
  , CN.Var 1208 "tmp"
  , CN.Seq 1208 1209
  , CN.Var 1209 "_i"
  , CN.Seq 1209 1210
  , CN.Branch 1210 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1211 1213
  , CN.Assign 1211 "tmp" (Num 0)
  , CN.Seq 1211 1212
  , CN.Assign 1212 "undefed" (Num 0)
  , CN.Seq 1212 1213
  , CN.Seq 1212 1210
  , CN.Var 1213 "LOOP_FOOTER"
  , CN.Seq 1213 1214
  , CN.Assign 1214 "stack_pointer" (Num 0)
  , CN.Seq 1214 1215
  , CN.Assign 1215 "stack_pointer" (Num 0)
  , CN.Seq 1215 1216
  , CN.Branch 1216 (Eq (Plus (Id "str_o") (Num 0)) (Num 1)) 1218 1218
  , CN.Seq 1217 1817
  , CN.Seq 1217 1219
  , CN.Var 1218 "NOP_1218"
  , CN.Seq 1218 1219
  , CN.Var 1219 "IF_ELSE_FOOTER"
  , CN.Assign 1220 "str" (Num 0)
  , CN.Seq 1220 1221
  , CN.Assign 1221 "undefed" (Num 0)
  , CN.Seq 1221 1222
  , CN.Assign 1222 "stack_pointer" (Num 0)
  , CN.Seq 1222 1223
  , CN.Var 1223 "word"
  , CN.Seq 1223 1224
  , CN.Assign 1224 "opcode" (Num 0)
  , CN.Seq 1224 1225
  , CN.Assign 1225 "oparg" (Num 0)
  , CN.Seq 1225 1226
  , CN.Branch 1226 (Eq (Num 0) (Num 1)) 1227 1230
  , CN.Var 1227 "word"
  , CN.Seq 1227 1228
  , CN.Assign 1228 "opcode" (Num 0)
  , CN.Seq 1228 1229
  , CN.Assign 1229 "oparg" (Num 0)
  , CN.Seq 1229 1230
  , CN.Seq 1229 1226
  , CN.Var 1230 "LOOP_FOOTER"
  , CN.Seq 1230 1231
  , CN.Seq 1230 35
  , CN.Branch 1231 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1233 1265
  , CN.Var 1233 "NOP_1233"
  , CN.Var 1234 "__CLABEL_TARGET_BUILD_TEMPLATE"
  , CN.Seq 1234 1235
  , CN.Assign 1235 "undefed" (Num 0)
  , CN.Seq 1235 1236
  , CN.Assign 1236 "next_instr" (Num 0)
  , CN.Seq 1236 1237
  , CN.Var 1237 "strings"
  , CN.Seq 1237 1238
  , CN.Var 1238 "interpolations"
  , CN.Seq 1238 1239
  , CN.Var 1239 "template"
  , CN.Seq 1239 1240
  , CN.Assign 1240 "interpolations" (Num 0)
  , CN.Seq 1240 1241
  , CN.Assign 1241 "strings" (Num 0)
  , CN.Seq 1241 1242
  , CN.Var 1242 "strings_o"
  , CN.Seq 1242 1243
  , CN.Var 1243 "interpolations_o"
  , CN.Seq 1243 1244
  , CN.Var 1244 "template_o"
  , CN.Seq 1244 1245
  , CN.Assign 1245 "stack_pointer" (Num 0)
  , CN.Seq 1245 1246
  , CN.Assign 1246 "stack_pointer" (Num 0)
  , CN.Seq 1246 1247
  , CN.Assign 1247 "stack_pointer" (Num 0)
  , CN.Seq 1247 1248
  , CN.Assign 1248 "stack_pointer" (Num 0)
  , CN.Seq 1248 1249
  , CN.Assign 1249 "stack_pointer" (Num 0)
  , CN.Seq 1249 1250
  , CN.Branch 1250 (Eq (Plus (Id "template_o") (Num 0)) (Num 1)) 1252 1252
  , CN.Seq 1251 1817
  , CN.Seq 1251 1253
  , CN.Var 1252 "NOP_1252"
  , CN.Seq 1252 1253
  , CN.Var 1253 "IF_ELSE_FOOTER"
  , CN.Assign 1254 "template" (Num 0)
  , CN.Seq 1254 1255
  , CN.Assign 1255 "undefed" (Num 0)
  , CN.Seq 1255 1256
  , CN.Assign 1256 "stack_pointer" (Num 0)
  , CN.Seq 1256 1257
  , CN.Var 1257 "word"
  , CN.Seq 1257 1258
  , CN.Assign 1258 "opcode" (Num 0)
  , CN.Seq 1258 1259
  , CN.Assign 1259 "oparg" (Num 0)
  , CN.Seq 1259 1260
  , CN.Branch 1260 (Eq (Num 0) (Num 1)) 1261 1264
  , CN.Var 1261 "word"
  , CN.Seq 1261 1262
  , CN.Assign 1262 "opcode" (Num 0)
  , CN.Seq 1262 1263
  , CN.Assign 1263 "oparg" (Num 0)
  , CN.Seq 1263 1264
  , CN.Seq 1263 1260
  , CN.Var 1264 "LOOP_FOOTER"
  , CN.Seq 1264 1265
  , CN.Seq 1264 35
  , CN.Branch 1265 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1267 1290
  , CN.Var 1267 "NOP_1267"
  , CN.Var 1268 "__CLABEL_TARGET_BUILD_TUPLE"
  , CN.Seq 1268 1269
  , CN.Assign 1269 "undefed" (Num 0)
  , CN.Seq 1269 1270
  , CN.Assign 1270 "next_instr" (Num 0)
  , CN.Seq 1270 1271
  , CN.Var 1271 "values"
  , CN.Seq 1271 1272
  , CN.Var 1272 "tup"
  , CN.Seq 1272 1273
  , CN.Assign 1273 "values" (Num 0)
  , CN.Seq 1273 1274
  , CN.Var 1274 "tup_o"
  , CN.Seq 1274 1275
  , CN.Branch 1275 (Eq (Plus (Id "tup_o") (Num 0)) (Num 1)) 1277 1277
  , CN.Seq 1276 1817
  , CN.Seq 1276 1278
  , CN.Var 1277 "NOP_1277"
  , CN.Seq 1277 1278
  , CN.Var 1278 "IF_ELSE_FOOTER"
  , CN.Assign 1279 "tup" (Num 0)
  , CN.Seq 1279 1280
  , CN.Assign 1280 "undefed" (Num 0)
  , CN.Seq 1280 1281
  , CN.Assign 1281 "stack_pointer" (Num 0)
  , CN.Seq 1281 1282
  , CN.Var 1282 "word"
  , CN.Seq 1282 1283
  , CN.Assign 1283 "opcode" (Num 0)
  , CN.Seq 1283 1284
  , CN.Assign 1284 "oparg" (Num 0)
  , CN.Seq 1284 1285
  , CN.Branch 1285 (Eq (Num 0) (Num 1)) 1286 1289
  , CN.Var 1286 "word"
  , CN.Seq 1286 1287
  , CN.Assign 1287 "opcode" (Num 0)
  , CN.Seq 1287 1288
  , CN.Assign 1288 "oparg" (Num 0)
  , CN.Seq 1288 1289
  , CN.Seq 1288 1285
  , CN.Var 1289 "LOOP_FOOTER"
  , CN.Seq 1289 1290
  , CN.Seq 1289 35
  , CN.Branch 1290 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1292 1304
  , CN.Var 1292 "NOP_1292"
  , CN.Var 1293 "__CLABEL_TARGET_CACHE"
  , CN.Seq 1293 1294
  , CN.Assign 1294 "undefed" (Num 0)
  , CN.Seq 1294 1295
  , CN.Assign 1295 "next_instr" (Num 0)
  , CN.Seq 1295 1296
  , CN.Var 1296 "word"
  , CN.Seq 1296 1297
  , CN.Assign 1297 "opcode" (Num 0)
  , CN.Seq 1297 1298
  , CN.Assign 1298 "oparg" (Num 0)
  , CN.Seq 1298 1299
  , CN.Branch 1299 (Eq (Num 0) (Num 1)) 1300 1303
  , CN.Var 1300 "word"
  , CN.Seq 1300 1301
  , CN.Assign 1301 "opcode" (Num 0)
  , CN.Seq 1301 1302
  , CN.Assign 1302 "oparg" (Num 0)
  , CN.Seq 1302 1303
  , CN.Seq 1302 1299
  , CN.Var 1303 "LOOP_FOOTER"
  , CN.Seq 1303 1304
  , CN.Seq 1303 35
  , CN.Branch 1304 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1306 1551
  , CN.Var 1306 "NOP_1306"
  , CN.Var 1307 "__CLABEL_TARGET_CALL"
  , CN.Seq 1307 1308
  , CN.Assign 1308 "undefed" (Num 0)
  , CN.Seq 1308 1309
  , CN.Assign 1309 "next_instr" (Num 0)
  , CN.Seq 1309 1310
  , CN.Var 1310 "__CLABEL_PREDICTED_CALL"
  , CN.Seq 1310 1311
  , CN.Var 1311 "NOP_1311"
  , CN.Var 1312 "this_instr"
  , CN.Seq 1312 1313
  , CN.Assign 1313 "opcode" (Num 0)
  , CN.Seq 1313 1314
  , CN.Var 1314 "callable"
  , CN.Seq 1314 1315
  , CN.Var 1315 "self_or_null"
  , CN.Seq 1315 1316
  , CN.Var 1316 "args"
  , CN.Seq 1316 1317
  , CN.Var 1317 "res"
  , CN.Seq 1317 1318
  , CN.Assign 1318 "self_or_null" (Num 0)
  , CN.Seq 1318 1319
  , CN.Assign 1319 "callable" (Num 0)
  , CN.Seq 1319 1320
  , CN.Var 1320 "counter"
  , CN.Seq 1320 1321
  , CN.Branch 1321 (Eq (Num 0) (Num 1)) 1323 1326
  , CN.Assign 1323 "next_instr" (Num 0)
  , CN.Seq 1323 1324
  , CN.Assign 1324 "stack_pointer" (Num 0)
  , CN.Seq 1324 1325
  , CN.Assign 1325 "opcode" (Num 0)
  , CN.Seq 1325 1326
  , CN.Seq 1325 35
  , CN.Seq 1325 1327
  , CN.Var 1326 "NOP_1326"
  , CN.Seq 1326 1327
  , CN.Var 1327 "IF_ELSE_FOOTER"
  , CN.Assign 1328 "undefed" (Num 0)
  , CN.Seq 1328 1329
  , CN.Branch 1329 (Eq (Num 0) (Num 1)) 1330 1331
  , CN.Assign 1330 "undefed" (Num 0)
  , CN.Seq 1330 1329
  , CN.Var 1331 "LOOP_FOOTER"
  , CN.Seq 1331 1332
  , CN.Branch 1332 (Eq (Plus (Plus (Num 0) (Plus (Id "PyMethod_Type") (Num 0))) (Plus (Num 0) (Num 0))) (Num 1)) 1334 1343
  , CN.Var 1334 "callable_o"
  , CN.Seq 1334 1335
  , CN.Var 1335 "self"
  , CN.Seq 1335 1336
  , CN.Assign 1336 "self_or_null" (Num 0)
  , CN.Seq 1336 1337
  , CN.Var 1337 "method"
  , CN.Seq 1337 1338
  , CN.Var 1338 "temp"
  , CN.Seq 1338 1339
  , CN.Assign 1339 "callable" (Num 0)
  , CN.Seq 1339 1340
  , CN.Assign 1340 "undefed" (Num 0)
  , CN.Seq 1340 1341
  , CN.Assign 1341 "undefed" (Num 0)
  , CN.Seq 1341 1342
  , CN.Assign 1342 "stack_pointer" (Num 0)
  , CN.Seq 1342 1343
  , CN.Seq 1342 1344
  , CN.Var 1343 "NOP_1343"
  , CN.Seq 1343 1344
  , CN.Var 1344 "IF_ELSE_FOOTER"
  , CN.Assign 1345 "args" (Num 0)
  , CN.Seq 1345 1346
  , CN.Var 1346 "callable_o"
  , CN.Seq 1346 1347
  , CN.Var 1347 "total_args"
  , CN.Seq 1347 1348
  , CN.Var 1348 "arguments"
  , CN.Seq 1348 1349
  , CN.Branch 1349 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1351 1352
  , CN.Assign 1351 "total_args" (Num 0)
  , CN.Seq 1351 1352
  , CN.Seq 1351 1353
  , CN.Var 1352 "NOP_1352"
  , CN.Seq 1352 1353
  , CN.Var 1353 "IF_ELSE_FOOTER"
  , CN.Branch 1354 (Eq (Plus (Plus (Plus (Num 0) (Plus (Id "PyFunction_Type") (Num 0))) (Plus (Num 0) (Num 0))) (Plus (Num 0) (Id "_PyFunction_Vectorcall"))) (Num 1)) 1356 1372
  , CN.Var 1356 "code_flags"
  , CN.Seq 1356 1357
  , CN.Var 1357 "locals"
  , CN.Seq 1357 1358
  , CN.Assign 1358 "undefed" (Num 0)
  , CN.Seq 1358 1359
  , CN.Assign 1359 "undefed" (Num 0)
  , CN.Seq 1359 1360
  , CN.Var 1360 "new_frame"
  , CN.Seq 1360 1361
  , CN.Assign 1361 "stack_pointer" (Num 0)
  , CN.Seq 1361 1362
  , CN.Assign 1362 "stack_pointer" (Num 0)
  , CN.Seq 1362 1363
  , CN.Branch 1363 (Eq (Plus (Id "new_frame") (Num 0)) (Num 1)) 1365 1365
  , CN.Seq 1364 1817
  , CN.Seq 1364 1366
  , CN.Var 1365 "NOP_1365"
  , CN.Seq 1365 1366
  , CN.Var 1366 "IF_ELSE_FOOTER"
  , CN.Assign 1367 "undefed" (Num 0)
  , CN.Seq 1367 1368
  , CN.Assign 1368 "frame" (Num 0)
  , CN.Seq 1368 1369
  , CN.Seq 1368 1886
  , CN.Branch 1369 (Eq (Num 0) (Num 1)) 1370 1371
  , CN.Assign 1370 "frame" (Num 0)
  , CN.Seq 1370 1371
  , CN.Seq 1370 1886
  , CN.Seq 1370 1369
  , CN.Var 1371 "LOOP_FOOTER"
  , CN.Seq 1371 1372
  , CN.Seq 1371 1373
  , CN.Var 1372 "NOP_1372"
  , CN.Seq 1372 1373
  , CN.Var 1373 "IF_ELSE_FOOTER"
  , CN.Var 1374 "args_o_temp"
  , CN.Seq 1374 1375
  , CN.Var 1375 "args_o"
  , CN.Seq 1375 1376
  , CN.Branch 1376 (Eq (Plus (Id "args_o") (Num 0)) (Num 1)) 1378 1394
  , CN.Var 1378 "tmp"
  , CN.Seq 1378 1379
  , CN.Var 1379 "_i"
  , CN.Seq 1379 1380
  , CN.Branch 1380 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1381 1385
  , CN.Assign 1381 "tmp" (Num 0)
  , CN.Seq 1381 1382
  , CN.Assign 1382 "undefed" (Num 0)
  , CN.Seq 1382 1383
  , CN.Assign 1383 "undefed" (Num 0)
  , CN.Seq 1383 1384
  , CN.Assign 1384 "undefed" (Num 0)
  , CN.Seq 1384 1385
  , CN.Seq 1384 1380
  , CN.Var 1385 "LOOP_FOOTER"
  , CN.Seq 1385 1386
  , CN.Assign 1386 "tmp" (Num 0)
  , CN.Seq 1386 1387
  , CN.Assign 1387 "self_or_null" (Num 0)
  , CN.Seq 1387 1388
  , CN.Assign 1388 "undefed" (Num 0)
  , CN.Seq 1388 1389
  , CN.Assign 1389 "tmp" (Num 0)
  , CN.Seq 1389 1390
  , CN.Assign 1390 "callable" (Num 0)
  , CN.Seq 1390 1391
  , CN.Assign 1391 "undefed" (Num 0)
  , CN.Seq 1391 1392
  , CN.Assign 1392 "stack_pointer" (Num 0)
  , CN.Seq 1392 1393
  , CN.Assign 1393 "stack_pointer" (Num 0)
  , CN.Seq 1393 1394
  , CN.Seq 1393 1817
  , CN.Seq 1393 1395
  , CN.Var 1394 "NOP_1394"
  , CN.Seq 1394 1395
  , CN.Var 1395 "IF_ELSE_FOOTER"
  , CN.Assign 1396 "undefed" (Num 0)
  , CN.Seq 1396 1397
  , CN.Assign 1397 "undefed" (Num 0)
  , CN.Seq 1397 1398
  , CN.Var 1398 "res_o"
  , CN.Seq 1398 1399
  , CN.Assign 1399 "stack_pointer" (Num 0)
  , CN.Seq 1399 1400
  , CN.Branch 1400 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1402 1514
  , CN.Var 1402 "arg"
  , CN.Seq 1402 1403
  , CN.Branch 1403 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 1405 1406
  , CN.Assign 1405 "stack_pointer" (Num 0)
  , CN.Seq 1405 1513
  , CN.Var 1406 "err"
  , CN.Seq 1406 1407
  , CN.Assign 1407 "stack_pointer" (Num 0)
  , CN.Seq 1407 1408
  , CN.Branch 1408 (Eq (Plus (Id "err") (Num 0)) (Num 1)) 1410 1511
  , CN.Assign 1410 "undefed" (Num 0)
  , CN.Seq 1410 1411
  , CN.Assign 1411 "_tmp_old_op" (Num 0)
  , CN.Seq 1411 1412
  , CN.Branch 1412 (Eq (Plus (Id "_tmp_old_op") (Num 0)) (Num 1)) 1414 1457
  , CN.Assign 1414 "undefed" (Num 0)
  , CN.Seq 1414 1415
  , CN.Var 1415 "op"
  , CN.Seq 1415 1416
  , CN.Branch 1416 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1418 1433
  , CN.Var 1418 "tracer"
  , CN.Seq 1418 1419
  , CN.Branch 1419 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1421 1422
  , CN.Var 1421 "data"
  , CN.Seq 1421 1422
  , CN.Seq 1421 1423
  , CN.Var 1422 "NOP_1422"
  , CN.Seq 1422 1423
  , CN.Var 1423 "IF_ELSE_FOOTER"
  , CN.Branch 1424 (Eq (Num 0) (Num 1)) 1425 1431
  , CN.Var 1425 "tracer"
  , CN.Seq 1425 1426
  , CN.Branch 1426 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1428 1429
  , CN.Var 1428 "data"
  , CN.Seq 1428 1429
  , CN.Seq 1428 1430
  , CN.Var 1429 "NOP_1429"
  , CN.Seq 1429 1430
  , CN.Var 1430 "IF_ELSE_FOOTER"
  , CN.Seq 1430 1424
  , CN.Var 1431 "LOOP_FOOTER"
  , CN.Seq 1431 1432
  , CN.Var 1432 "dealloc"
  , CN.Seq 1432 1433
  , CN.Seq 1432 1434
  , CN.Var 1433 "NOP_1433"
  , CN.Seq 1433 1434
  , CN.Var 1434 "IF_ELSE_FOOTER"
  , CN.Branch 1435 (Eq (Num 0) (Num 1)) 1436 1456
  , CN.Var 1436 "op"
  , CN.Seq 1436 1437
  , CN.Branch 1437 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1439 1454
  , CN.Var 1439 "tracer"
  , CN.Seq 1439 1440
  , CN.Branch 1440 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1442 1443
  , CN.Var 1442 "data"
  , CN.Seq 1442 1443
  , CN.Seq 1442 1444
  , CN.Var 1443 "NOP_1443"
  , CN.Seq 1443 1444
  , CN.Var 1444 "IF_ELSE_FOOTER"
  , CN.Branch 1445 (Eq (Num 0) (Num 1)) 1446 1452
  , CN.Var 1446 "tracer"
  , CN.Seq 1446 1447
  , CN.Branch 1447 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1449 1450
  , CN.Var 1449 "data"
  , CN.Seq 1449 1450
  , CN.Seq 1449 1451
  , CN.Var 1450 "NOP_1450"
  , CN.Seq 1450 1451
  , CN.Var 1451 "IF_ELSE_FOOTER"
  , CN.Seq 1451 1445
  , CN.Var 1452 "LOOP_FOOTER"
  , CN.Seq 1452 1453
  , CN.Var 1453 "dealloc"
  , CN.Seq 1453 1454
  , CN.Seq 1453 1455
  , CN.Var 1454 "NOP_1454"
  , CN.Seq 1454 1455
  , CN.Var 1455 "IF_ELSE_FOOTER"
  , CN.Seq 1455 1435
  , CN.Var 1456 "LOOP_FOOTER"
  , CN.Seq 1456 1457
  , CN.Seq 1456 1458
  , CN.Var 1457 "NOP_1457"
  , CN.Seq 1457 1458
  , CN.Var 1458 "IF_ELSE_FOOTER"
  , CN.Branch 1459 (Eq (Num 0) (Num 1)) 1460 1509
  , CN.Assign 1460 "undefed" (Num 0)
  , CN.Seq 1460 1461
  , CN.Assign 1461 "_tmp_old_op" (Num 0)
  , CN.Seq 1461 1462
  , CN.Branch 1462 (Eq (Plus (Id "_tmp_old_op") (Num 0)) (Num 1)) 1464 1507
  , CN.Assign 1464 "undefed" (Num 0)
  , CN.Seq 1464 1465
  , CN.Var 1465 "op"
  , CN.Seq 1465 1466
  , CN.Branch 1466 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1468 1483
  , CN.Var 1468 "tracer"
  , CN.Seq 1468 1469
  , CN.Branch 1469 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1471 1472
  , CN.Var 1471 "data"
  , CN.Seq 1471 1472
  , CN.Seq 1471 1473
  , CN.Var 1472 "NOP_1472"
  , CN.Seq 1472 1473
  , CN.Var 1473 "IF_ELSE_FOOTER"
  , CN.Branch 1474 (Eq (Num 0) (Num 1)) 1475 1481
  , CN.Var 1475 "tracer"
  , CN.Seq 1475 1476
  , CN.Branch 1476 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1478 1479
  , CN.Var 1478 "data"
  , CN.Seq 1478 1479
  , CN.Seq 1478 1480
  , CN.Var 1479 "NOP_1479"
  , CN.Seq 1479 1480
  , CN.Var 1480 "IF_ELSE_FOOTER"
  , CN.Seq 1480 1474
  , CN.Var 1481 "LOOP_FOOTER"
  , CN.Seq 1481 1482
  , CN.Var 1482 "dealloc"
  , CN.Seq 1482 1483
  , CN.Seq 1482 1484
  , CN.Var 1483 "NOP_1483"
  , CN.Seq 1483 1484
  , CN.Var 1484 "IF_ELSE_FOOTER"
  , CN.Branch 1485 (Eq (Num 0) (Num 1)) 1486 1506
  , CN.Var 1486 "op"
  , CN.Seq 1486 1487
  , CN.Branch 1487 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1489 1504
  , CN.Var 1489 "tracer"
  , CN.Seq 1489 1490
  , CN.Branch 1490 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1492 1493
  , CN.Var 1492 "data"
  , CN.Seq 1492 1493
  , CN.Seq 1492 1494
  , CN.Var 1493 "NOP_1493"
  , CN.Seq 1493 1494
  , CN.Var 1494 "IF_ELSE_FOOTER"
  , CN.Branch 1495 (Eq (Num 0) (Num 1)) 1496 1502
  , CN.Var 1496 "tracer"
  , CN.Seq 1496 1497
  , CN.Branch 1497 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1499 1500
  , CN.Var 1499 "data"
  , CN.Seq 1499 1500
  , CN.Seq 1499 1501
  , CN.Var 1500 "NOP_1500"
  , CN.Seq 1500 1501
  , CN.Var 1501 "IF_ELSE_FOOTER"
  , CN.Seq 1501 1495
  , CN.Var 1502 "LOOP_FOOTER"
  , CN.Seq 1502 1503
  , CN.Var 1503 "dealloc"
  , CN.Seq 1503 1504
  , CN.Seq 1503 1505
  , CN.Var 1504 "NOP_1504"
  , CN.Seq 1504 1505
  , CN.Var 1505 "IF_ELSE_FOOTER"
  , CN.Seq 1505 1485
  , CN.Var 1506 "LOOP_FOOTER"
  , CN.Seq 1506 1507
  , CN.Seq 1506 1508
  , CN.Var 1507 "NOP_1507"
  , CN.Seq 1507 1508
  , CN.Var 1508 "IF_ELSE_FOOTER"
  , CN.Seq 1508 1459
  , CN.Var 1509 "LOOP_FOOTER"
  , CN.Seq 1509 1510
  , CN.Assign 1510 "stack_pointer" (Num 0)
  , CN.Seq 1510 1511
  , CN.Seq 1510 1512
  , CN.Var 1511 "NOP_1511"
  , CN.Seq 1511 1512
  , CN.Var 1512 "IF_ELSE_FOOTER"
  , CN.Var 1513 "IF_ELSE_FOOTER"
  , CN.Seq 1513 1515
  , CN.Var 1514 "NOP_1514"
  , CN.Seq 1514 1515
  , CN.Var 1515 "IF_ELSE_FOOTER"
  , CN.Var 1516 "tmp"
  , CN.Seq 1516 1517
  , CN.Var 1517 "_i"
  , CN.Seq 1517 1518
  , CN.Branch 1518 (Eq (Plus (Plus (Id "_i") (Num 0)) (Num 0)) (Num 1)) 1519 1521
  , CN.Assign 1519 "tmp" (Num 0)
  , CN.Seq 1519 1520
  , CN.Assign 1520 "undefed" (Num 0)
  , CN.Seq 1520 1521
  , CN.Seq 1520 1518
  , CN.Var 1521 "LOOP_FOOTER"
  , CN.Seq 1521 1522
  , CN.Assign 1522 "tmp" (Num 0)
  , CN.Seq 1522 1523
  , CN.Assign 1523 "self_or_null" (Num 0)
  , CN.Seq 1523 1524
  , CN.Assign 1524 "undefed" (Num 0)
  , CN.Seq 1524 1525
  , CN.Assign 1525 "tmp" (Num 0)
  , CN.Seq 1525 1526
  , CN.Assign 1526 "callable" (Num 0)
  , CN.Seq 1526 1527
  , CN.Assign 1527 "undefed" (Num 0)
  , CN.Seq 1527 1528
  , CN.Assign 1528 "stack_pointer" (Num 0)
  , CN.Seq 1528 1529
  , CN.Assign 1529 "stack_pointer" (Num 0)
  , CN.Seq 1529 1530
  , CN.Branch 1530 (Eq (Plus (Id "res_o") (Num 0)) (Num 1)) 1532 1532
  , CN.Seq 1531 1817
  , CN.Seq 1531 1533
  , CN.Var 1532 "NOP_1532"
  , CN.Seq 1532 1533
  , CN.Var 1533 "IF_ELSE_FOOTER"
  , CN.Assign 1534 "res" (Num 0)
  , CN.Seq 1534 1535
  , CN.Assign 1535 "undefed" (Num 0)
  , CN.Seq 1535 1536
  , CN.Assign 1536 "stack_pointer" (Num 0)
  , CN.Seq 1536 1537
  , CN.Var 1537 "err"
  , CN.Seq 1537 1538
  , CN.Assign 1538 "stack_pointer" (Num 0)
  , CN.Seq 1538 1539
  , CN.Branch 1539 (Eq (Plus (Id "err") (Num 0)) (Num 1)) 1541 1541
  , CN.Seq 1540 1817
  , CN.Seq 1540 1542
  , CN.Var 1541 "NOP_1541"
  , CN.Seq 1541 1542
  , CN.Var 1542 "IF_ELSE_FOOTER"
  , CN.Var 1543 "word"
  , CN.Seq 1543 1544
  , CN.Assign 1544 "opcode" (Num 0)
  , CN.Seq 1544 1545
  , CN.Assign 1545 "oparg" (Num 0)
  , CN.Seq 1545 1546
  , CN.Branch 1546 (Eq (Num 0) (Num 1)) 1547 1550
  , CN.Var 1547 "word"
  , CN.Seq 1547 1548
  , CN.Assign 1548 "opcode" (Num 0)
  , CN.Seq 1548 1549
  , CN.Assign 1549 "oparg" (Num 0)
  , CN.Seq 1549 1550
  , CN.Seq 1549 1546
  , CN.Var 1550 "LOOP_FOOTER"
  , CN.Seq 1550 1551
  , CN.Seq 1550 35
  , CN.Branch 1551 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1553 1637
  , CN.Var 1553 "NOP_1553"
  , CN.Var 1554 "__CLABEL_TARGET_CALL_ALLOC_AND_ENTER_INIT"
  , CN.Seq 1554 1555
  , CN.Var 1555 "this_instr"
  , CN.Seq 1555 1556
  , CN.Assign 1556 "undefed" (Num 0)
  , CN.Seq 1556 1557
  , CN.Assign 1557 "next_instr" (Num 0)
  , CN.Seq 1557 1558
  , CN.Var 1558 "callable"
  , CN.Seq 1558 1559
  , CN.Var 1559 "self_or_null"
  , CN.Seq 1559 1560
  , CN.Var 1560 "init"
  , CN.Seq 1560 1561
  , CN.Var 1561 "self"
  , CN.Seq 1561 1562
  , CN.Var 1562 "args"
  , CN.Seq 1562 1563
  , CN.Var 1563 "init_frame"
  , CN.Seq 1563 1564
  , CN.Var 1564 "new_frame"
  , CN.Seq 1564 1565
  , CN.Branch 1565 (Eq (Num 0) (Num 1)) 1567 1567
  , CN.Seq 1566 1310
  , CN.Seq 1566 1568
  , CN.Var 1567 "NOP_1567"
  , CN.Seq 1567 1568
  , CN.Var 1568 "IF_ELSE_FOOTER"
  , CN.Assign 1569 "self_or_null" (Num 0)
  , CN.Seq 1569 1570
  , CN.Assign 1570 "callable" (Num 0)
  , CN.Seq 1570 1571
  , CN.Var 1571 "type_version"
  , CN.Seq 1571 1572
  , CN.Var 1572 "callable_o"
  , CN.Seq 1572 1573
  , CN.Branch 1573 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1575 1575
  , CN.Seq 1574 1310
  , CN.Seq 1574 1576
  , CN.Var 1575 "NOP_1575"
  , CN.Seq 1575 1576
  , CN.Var 1576 "IF_ELSE_FOOTER"
  , CN.Branch 1577 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1579 1579
  , CN.Seq 1578 1310
  , CN.Seq 1578 1580
  , CN.Var 1579 "NOP_1579"
  , CN.Seq 1579 1580
  , CN.Var 1580 "IF_ELSE_FOOTER"
  , CN.Var 1581 "tp"
  , CN.Seq 1581 1582
  , CN.Branch 1582 (Eq (Plus (Num 0) (Id "type_version")) (Num 1)) 1584 1584
  , CN.Seq 1583 1310
  , CN.Seq 1583 1585
  , CN.Var 1584 "NOP_1584"
  , CN.Seq 1584 1585
  , CN.Var 1585 "IF_ELSE_FOOTER"
  , CN.Var 1586 "cls"
  , CN.Seq 1586 1587
  , CN.Var 1587 "init_func"
  , CN.Seq 1587 1588
  , CN.Var 1588 "code"
  , CN.Seq 1588 1589
  , CN.Branch 1589 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1591 1591
  , CN.Seq 1590 1310
  , CN.Seq 1590 1592
  , CN.Var 1591 "NOP_1591"
  , CN.Seq 1591 1592
  , CN.Var 1592 "IF_ELSE_FOOTER"
  , CN.Var 1593 "self_o"
  , CN.Seq 1593 1594
  , CN.Assign 1594 "stack_pointer" (Num 0)
  , CN.Seq 1594 1595
  , CN.Branch 1595 (Eq (Plus (Id "self_o") (Num 0)) (Num 1)) 1597 1597
  , CN.Seq 1596 1817
  , CN.Seq 1596 1598
  , CN.Var 1597 "NOP_1597"
  , CN.Seq 1597 1598
  , CN.Var 1598 "IF_ELSE_FOOTER"
  , CN.Assign 1599 "self_or_null" (Num 0)
  , CN.Seq 1599 1600
  , CN.Var 1600 "temp"
  , CN.Seq 1600 1601
  , CN.Assign 1601 "callable" (Num 0)
  , CN.Seq 1601 1602
  , CN.Assign 1602 "undefed" (Num 0)
  , CN.Seq 1602 1603
  , CN.Assign 1603 "undefed" (Num 0)
  , CN.Seq 1603 1604
  , CN.Assign 1604 "stack_pointer" (Num 0)
  , CN.Seq 1604 1605
  , CN.Assign 1605 "args" (Num 0)
  , CN.Seq 1605 1606
  , CN.Assign 1606 "self" (Num 0)
  , CN.Seq 1606 1607
  , CN.Assign 1607 "init" (Num 0)
  , CN.Seq 1607 1608
  , CN.Var 1608 "shim"
  , CN.Seq 1608 1609
  , CN.Assign 1609 "stack_pointer" (Num 0)
  , CN.Seq 1609 1610
  , CN.Assign 1610 "undefed" (Num 0)
  , CN.Seq 1610 1611
  , CN.Var 1611 "temp"
  , CN.Seq 1611 1612
  , CN.Assign 1612 "stack_pointer" (Num 0)
  , CN.Seq 1612 1613
  , CN.Assign 1613 "stack_pointer" (Num 0)
  , CN.Seq 1613 1614
  , CN.Branch 1614 (Eq (Plus (Id "temp") (Num 0)) (Num 1)) 1616 1617
  , CN.Assign 1616 "stack_pointer" (Num 0)
  , CN.Seq 1616 1617
  , CN.Seq 1616 1817
  , CN.Seq 1616 1618
  , CN.Var 1617 "NOP_1617"
  , CN.Seq 1617 1618
  , CN.Var 1618 "IF_ELSE_FOOTER"
  , CN.Assign 1619 "undefed" (Num 0)
  , CN.Seq 1619 1620
  , CN.Assign 1620 "init_frame" (Num 0)
  , CN.Seq 1620 1621
  , CN.Assign 1621 "new_frame" (Num 0)
  , CN.Seq 1621 1622
  , CN.Var 1622 "temp"
  , CN.Seq 1622 1623
  , CN.Assign 1623 "frame" (Num 0)
  , CN.Seq 1623 1624
  , CN.Assign 1624 "stack_pointer" (Num 0)
  , CN.Seq 1624 1625
  , CN.Assign 1625 "next_instr" (Num 0)
  , CN.Seq 1625 1626
  , CN.Branch 1626 (Eq (Num 0) (Num 1)) 1627 1628
  , CN.Assign 1627 "next_instr" (Num 0)
  , CN.Seq 1627 1626
  , CN.Var 1628 "LOOP_FOOTER"
  , CN.Seq 1628 1629
  , CN.Var 1629 "word"
  , CN.Seq 1629 1630
  , CN.Assign 1630 "opcode" (Num 0)
  , CN.Seq 1630 1631
  , CN.Assign 1631 "oparg" (Num 0)
  , CN.Seq 1631 1632
  , CN.Branch 1632 (Eq (Num 0) (Num 1)) 1633 1636
  , CN.Var 1633 "word"
  , CN.Seq 1633 1634
  , CN.Assign 1634 "opcode" (Num 0)
  , CN.Seq 1634 1635
  , CN.Assign 1635 "oparg" (Num 0)
  , CN.Seq 1635 1636
  , CN.Seq 1635 1632
  , CN.Var 1636 "LOOP_FOOTER"
  , CN.Seq 1636 1637
  , CN.Seq 1636 35
  , CN.Branch 1637 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1639 1727
  , CN.Var 1639 "NOP_1639"
  , CN.Var 1640 "__CLABEL_TARGET_CALL_BOUND_METHOD_EXACT_ARGS"
  , CN.Seq 1640 1641
  , CN.Var 1641 "this_instr"
  , CN.Seq 1641 1642
  , CN.Assign 1642 "undefed" (Num 0)
  , CN.Seq 1642 1643
  , CN.Assign 1643 "next_instr" (Num 0)
  , CN.Seq 1643 1644
  , CN.Var 1644 "callable"
  , CN.Seq 1644 1645
  , CN.Var 1645 "null"
  , CN.Seq 1645 1646
  , CN.Var 1646 "self_or_null"
  , CN.Seq 1646 1647
  , CN.Var 1647 "args"
  , CN.Seq 1647 1648
  , CN.Var 1648 "new_frame"
  , CN.Seq 1648 1649
  , CN.Branch 1649 (Eq (Num 0) (Num 1)) 1651 1651
  , CN.Seq 1650 1310
  , CN.Seq 1650 1652
  , CN.Var 1651 "NOP_1651"
  , CN.Seq 1651 1652
  , CN.Var 1652 "IF_ELSE_FOOTER"
  , CN.Assign 1653 "null" (Num 0)
  , CN.Seq 1653 1654
  , CN.Assign 1654 "callable" (Num 0)
  , CN.Seq 1654 1655
  , CN.Branch 1655 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1657 1657
  , CN.Seq 1656 1310
  , CN.Seq 1656 1658
  , CN.Var 1657 "NOP_1657"
  , CN.Seq 1657 1658
  , CN.Var 1658 "IF_ELSE_FOOTER"
  , CN.Branch 1659 (Eq (Plus (Num 0) (Plus (Id "PyMethod_Type") (Num 0))) (Num 1)) 1661 1661
  , CN.Seq 1660 1310
  , CN.Seq 1660 1662
  , CN.Var 1661 "NOP_1661"
  , CN.Seq 1661 1662
  , CN.Var 1662 "IF_ELSE_FOOTER"
  , CN.Assign 1663 "self_or_null" (Num 0)
  , CN.Seq 1663 1664
  , CN.Var 1664 "callable_o"
  , CN.Seq 1664 1665
  , CN.Assign 1665 "self_or_null" (Num 0)
  , CN.Seq 1665 1666
  , CN.Var 1666 "temp"
  , CN.Seq 1666 1667
  , CN.Assign 1667 "callable" (Num 0)
  , CN.Seq 1667 1668
  , CN.Assign 1668 "undefed" (Num 0)
  , CN.Seq 1668 1669
  , CN.Assign 1669 "undefed" (Num 0)
  , CN.Seq 1669 1670
  , CN.Assign 1670 "stack_pointer" (Num 0)
  , CN.Seq 1670 1671
  , CN.Var 1671 "func_version"
  , CN.Seq 1671 1672
  , CN.Var 1672 "callable_o"
  , CN.Seq 1672 1673
  , CN.Branch 1673 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFunction_Type") (Num 0))) (Num 0)) (Num 1)) 1675 1675
  , CN.Seq 1674 1310
  , CN.Seq 1674 1676
  , CN.Var 1675 "NOP_1675"
  , CN.Seq 1675 1676
  , CN.Var 1676 "IF_ELSE_FOOTER"
  , CN.Var 1677 "func"
  , CN.Seq 1677 1678
  , CN.Branch 1678 (Eq (Plus (Num 0) (Id "func_version")) (Num 1)) 1680 1680
  , CN.Seq 1679 1310
  , CN.Seq 1679 1681
  , CN.Var 1680 "NOP_1680"
  , CN.Seq 1680 1681
  , CN.Var 1681 "IF_ELSE_FOOTER"
  , CN.Var 1682 "callable_o"
  , CN.Seq 1682 1683
  , CN.Var 1683 "func"
  , CN.Seq 1683 1684
  , CN.Var 1684 "code"
  , CN.Seq 1684 1685
  , CN.Branch 1685 (Eq (Plus (Num 0) (Plus (Id "oparg") (Plus (Plus (Num 0) (Num 0)) (Num 0)))) (Num 1)) 1687 1687
  , CN.Seq 1686 1310
  , CN.Seq 1686 1688
  , CN.Var 1687 "NOP_1687"
  , CN.Seq 1687 1688
  , CN.Var 1688 "IF_ELSE_FOOTER"
  , CN.Var 1689 "callable_o"
  , CN.Seq 1689 1690
  , CN.Var 1690 "func"
  , CN.Seq 1690 1691
  , CN.Var 1691 "code"
  , CN.Seq 1691 1692
  , CN.Branch 1692 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1694 1694
  , CN.Seq 1693 1310
  , CN.Seq 1693 1695
  , CN.Var 1694 "NOP_1694"
  , CN.Seq 1694 1695
  , CN.Var 1695 "IF_ELSE_FOOTER"
  , CN.Branch 1696 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1698 1698
  , CN.Seq 1697 1310
  , CN.Seq 1697 1699
  , CN.Var 1698 "NOP_1698"
  , CN.Seq 1698 1699
  , CN.Var 1699 "IF_ELSE_FOOTER"
  , CN.Assign 1700 "args" (Num 0)
  , CN.Seq 1700 1701
  , CN.Var 1701 "has_self"
  , CN.Seq 1701 1702
  , CN.Var 1702 "pushed_frame"
  , CN.Seq 1702 1703
  , CN.Var 1703 "first_non_self_local"
  , CN.Seq 1703 1704
  , CN.Assign 1704 "undefed" (Num 0)
  , CN.Seq 1704 1705
  , CN.Var 1705 "i"
  , CN.Seq 1705 1706
  , CN.Branch 1706 (Eq (Plus (Id "i") (Id "oparg")) (Num 1)) 1707 1708
  , CN.Assign 1707 "undefed" (Num 0)
  , CN.Seq 1707 1708
  , CN.Seq 1707 1706
  , CN.Var 1708 "LOOP_FOOTER"
  , CN.Seq 1708 1709
  , CN.Assign 1709 "new_frame" (Num 0)
  , CN.Seq 1709 1710
  , CN.Assign 1710 "undefed" (Num 0)
  , CN.Seq 1710 1711
  , CN.Var 1711 "temp"
  , CN.Seq 1711 1712
  , CN.Assign 1712 "stack_pointer" (Num 0)
  , CN.Seq 1712 1713
  , CN.Assign 1713 "frame" (Num 0)
  , CN.Seq 1713 1714
  , CN.Assign 1714 "stack_pointer" (Num 0)
  , CN.Seq 1714 1715
  , CN.Assign 1715 "next_instr" (Num 0)
  , CN.Seq 1715 1716
  , CN.Branch 1716 (Eq (Num 0) (Num 1)) 1717 1718
  , CN.Assign 1717 "next_instr" (Num 0)
  , CN.Seq 1717 1716
  , CN.Var 1718 "LOOP_FOOTER"
  , CN.Seq 1718 1719
  , CN.Var 1719 "word"
  , CN.Seq 1719 1720
  , CN.Assign 1720 "opcode" (Num 0)
  , CN.Seq 1720 1721
  , CN.Assign 1721 "oparg" (Num 0)
  , CN.Seq 1721 1722
  , CN.Branch 1722 (Eq (Num 0) (Num 1)) 1723 1726
  , CN.Var 1723 "word"
  , CN.Seq 1723 1724
  , CN.Assign 1724 "opcode" (Num 0)
  , CN.Seq 1724 1725
  , CN.Assign 1725 "oparg" (Num 0)
  , CN.Seq 1725 1726
  , CN.Seq 1725 1722
  , CN.Var 1726 "LOOP_FOOTER"
  , CN.Seq 1726 1727
  , CN.Seq 1726 35
  , CN.Branch 1727 (Eq (Plus (Id "opcode") (Num 0)) (Num 1)) 1729 1810
  , CN.Var 1729 "NOP_1729"
  , CN.Var 1730 "__CLABEL_TARGET_CALL_BOUND_METHOD_GENERAL"
  , CN.Seq 1730 1731
  , CN.Var 1731 "this_instr"
  , CN.Seq 1731 1732
  , CN.Assign 1732 "undefed" (Num 0)
  , CN.Seq 1732 1733
  , CN.Assign 1733 "next_instr" (Num 0)
  , CN.Seq 1733 1734
  , CN.Var 1734 "callable"
  , CN.Seq 1734 1735
  , CN.Var 1735 "null"
  , CN.Seq 1735 1736
  , CN.Var 1736 "self_or_null"
  , CN.Seq 1736 1737
  , CN.Var 1737 "args"
  , CN.Seq 1737 1738
  , CN.Var 1738 "new_frame"
  , CN.Seq 1738 1739
  , CN.Branch 1739 (Eq (Num 0) (Num 1)) 1741 1741
  , CN.Seq 1740 1310
  , CN.Seq 1740 1742
  , CN.Var 1741 "NOP_1741"
  , CN.Seq 1741 1742
  , CN.Var 1742 "IF_ELSE_FOOTER"
  , CN.Assign 1743 "null" (Num 0)
  , CN.Seq 1743 1744
  , CN.Assign 1744 "callable" (Num 0)
  , CN.Seq 1744 1745
  , CN.Var 1745 "func_version"
  , CN.Seq 1745 1746
  , CN.Var 1746 "callable_o"
  , CN.Seq 1746 1747
  , CN.Branch 1747 (Eq (Plus (Num 0) (Plus (Id "PyMethod_Type") (Num 0))) (Num 1)) 1749 1749
  , CN.Seq 1748 1310
  , CN.Seq 1748 1750
  , CN.Var 1749 "NOP_1749"
  , CN.Seq 1749 1750
  , CN.Var 1750 "IF_ELSE_FOOTER"
  , CN.Var 1751 "func"
  , CN.Seq 1751 1752
  , CN.Branch 1752 (Eq (Plus (Plus (Num 0) (Plus (Id "PyFunction_Type") (Num 0))) (Num 0)) (Num 1)) 1754 1754
  , CN.Seq 1753 1310
  , CN.Seq 1753 1755
  , CN.Var 1754 "NOP_1754"
  , CN.Seq 1754 1755
  , CN.Var 1755 "IF_ELSE_FOOTER"
  , CN.Branch 1756 (Eq (Plus (Num 0) (Id "func_version")) (Num 1)) 1758 1758
  , CN.Seq 1757 1310
  , CN.Seq 1757 1759
  , CN.Var 1758 "NOP_1758"
  , CN.Seq 1758 1759
  , CN.Var 1759 "IF_ELSE_FOOTER"
  , CN.Branch 1760 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1762 1762
  , CN.Seq 1761 1310
  , CN.Seq 1761 1763
  , CN.Var 1762 "NOP_1762"
  , CN.Seq 1762 1763
  , CN.Var 1763 "IF_ELSE_FOOTER"
  , CN.Assign 1764 "self_or_null" (Num 0)
  , CN.Seq 1764 1765
  , CN.Var 1765 "callable_o"
  , CN.Seq 1765 1766
  , CN.Assign 1766 "self_or_null" (Num 0)
  , CN.Seq 1766 1767
  , CN.Var 1767 "temp"
  , CN.Seq 1767 1768
  , CN.Assign 1768 "callable" (Num 0)
  , CN.Seq 1768 1769
  , CN.Assign 1769 "undefed" (Num 0)
  , CN.Seq 1769 1770
  , CN.Assign 1770 "undefed" (Num 0)
  , CN.Seq 1770 1771
  , CN.Assign 1771 "stack_pointer" (Num 0)
  , CN.Seq 1771 1772
  , CN.Branch 1772 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1774 1774
  , CN.Seq 1773 1310
  , CN.Seq 1773 1775
  , CN.Var 1774 "NOP_1774"
  , CN.Seq 1774 1775
  , CN.Var 1775 "IF_ELSE_FOOTER"
  , CN.Assign 1776 "args" (Num 0)
  , CN.Seq 1776 1777
  , CN.Var 1777 "callable_o"
  , CN.Seq 1777 1778
  , CN.Var 1778 "total_args"
  , CN.Seq 1778 1779
  , CN.Branch 1779 (Eq (Plus (Plus (Num 0) (Num 0)) (Num 0)) (Num 1)) 1781 1782
  , CN.Assign 1781 "total_args" (Num 0)
  , CN.Seq 1781 1782
  , CN.Seq 1781 1783
  , CN.Var 1782 "NOP_1782"
  , CN.Seq 1782 1783
  , CN.Var 1783 "IF_ELSE_FOOTER"
  , CN.Var 1784 "code_flags"
  , CN.Seq 1784 1785
  , CN.Var 1785 "locals"
  , CN.Seq 1785 1786
  , CN.Var 1786 "temp"
  , CN.Seq 1786 1787
  , CN.Assign 1787 "stack_pointer" (Num 0)
  , CN.Seq 1787 1788
  , CN.Assign 1788 "stack_pointer" (Num 0)
  , CN.Seq 1788 1789
  , CN.Branch 1789 (Eq (Plus (Id "temp") (Num 0)) (Num 1)) 1791 1791
  , CN.Seq 1790 1817
  , CN.Seq 1790 1792
  , CN.Var 1791 "NOP_1791"
  , CN.Seq 1791 1792
  , CN.Var 1792 "IF_ELSE_FOOTER"
  , CN.Assign 1793 "new_frame" (Num 0)
  , CN.Seq 1793 1794
  , CN.Assign 1794 "undefed" (Num 0)
  , CN.Seq 1794 1795
  , CN.Var 1795 "temp"
  , CN.Seq 1795 1796
  , CN.Assign 1796 "frame" (Num 0)
  , CN.Seq 1796 1797
  , CN.Assign 1797 "stack_pointer" (Num 0)
  , CN.Seq 1797 1798
  , CN.Assign 1798 "next_instr" (Num 0)
  , CN.Seq 1798 1799
  , CN.Branch 1799 (Eq (Num 0) (Num 1)) 1800 1801
  , CN.Assign 1800 "next_instr" (Num 0)
  , CN.Seq 1800 1799
  , CN.Var 1801 "LOOP_FOOTER"
  , CN.Seq 1801 1802
  , CN.Var 1802 "word"
  , CN.Seq 1802 1803
  , CN.Assign 1803 "opcode" (Num 0)
  , CN.Seq 1803 1804
  , CN.Assign 1804 "oparg" (Num 0)
  , CN.Seq 1804 1805
  , CN.Branch 1805 (Eq (Num 0) (Num 1)) 1806 1809
  , CN.Var 1806 "word"
  , CN.Seq 1806 1807
  , CN.Assign 1807 "opcode" (Num 0)
  , CN.Seq 1807 1808
  , CN.Assign 1808 "oparg" (Num 0)
  , CN.Seq 1808 1809
  , CN.Seq 1808 1805
  , CN.Var 1809 "LOOP_FOOTER"
  , CN.Seq 1809 1810
  , CN.Seq 1809 35
  , CN.Var 1810 "NOP_1810"
  , CN.Seq 1810 1811
  , CN.Var 1811 "__CLABEL_CODEGEN_SWITCH_EXIT_0"
  , CN.Seq 1811 1812
  , CN.Var 1812 "NOP_1812"
  , CN.Var 1813 "__CLABEL_pop_2_error"
  , CN.Seq 1813 1814
  , CN.Assign 1814 "stack_pointer" (Num 0)
  , CN.Seq 1814 1815
  , CN.Seq 1814 1817
  , CN.Var 1815 "__CLABEL_pop_1_error"
  , CN.Seq 1815 1816
  , CN.Assign 1816 "stack_pointer" (Num 0)
  , CN.Seq 1816 1817
  , CN.Seq 1816 1817
  , CN.Var 1817 "__CLABEL_error"
  , CN.Seq 1817 1818
  , CN.Branch 1818 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1820 1821
  , CN.Assign 1820 "stack_pointer" (Num 0)
  , CN.Seq 1820 1822
  , CN.Var 1821 "NOP_1821"
  , CN.Seq 1821 1822
  , CN.Var 1822 "IF_ELSE_FOOTER"
  , CN.Branch 1823 (Eq (Plus (Num 0) (Num 0)) (Num 1)) 1825 1832
  , CN.Var 1825 "f"
  , CN.Seq 1825 1826
  , CN.Assign 1826 "stack_pointer" (Num 0)
  , CN.Seq 1826 1827
  , CN.Branch 1827 (Eq (Plus (Id "f") (Num 0)) (Num 1)) 1829 1830
  , CN.Assign 1829 "stack_pointer" (Num 0)
  , CN.Seq 1829 1831
  , CN.Var 1830 "NOP_1830"
  , CN.Seq 1830 1831
  , CN.Var 1831 "IF_ELSE_FOOTER"
  , CN.Seq 1831 1833
  , CN.Var 1832 "NOP_1832"
  , CN.Seq 1832 1833
  , CN.Var 1833 "IF_ELSE_FOOTER"
  , CN.Seq 1833 1834
  , CN.Var 1834 "__CLABEL_exception_unwind"
  , CN.Seq 1834 1835
  , CN.Var 1835 "offset"
  , CN.Seq 1835 1836
  , CN.Var 1836 "level"
  , CN.Seq 1836 1837
  , CN.Var 1837 "handler"
  , CN.Seq 1837 1838
  , CN.Var 1838 "lasti"
  , CN.Seq 1838 1839
  , CN.Var 1839 "handled"
  , CN.Seq 1839 1840
  , CN.Branch 1840 (Eq (Plus (Id "handled") (Num 0)) (Num 1)) 1842 1846
  , CN.Var 1842 "stackbase"
  , CN.Seq 1842 1843
  , CN.Branch 1843 (Eq (Plus (Num 0) (Id "stackbase")) (Num 1)) 1844 1845
  , CN.Var 1844 "ref"
  , CN.Seq 1844 1845
  , CN.Seq 1844 1843
  , CN.Var 1845 "LOOP_FOOTER"
  , CN.Seq 1845 1846
  , CN.Seq 1845 1874
  , CN.Seq 1845 1847
  , CN.Var 1846 "NOP_1846"
  , CN.Seq 1846 1847
  , CN.Var 1847 "IF_ELSE_FOOTER"
  , CN.Var 1848 "new_top"
  , CN.Seq 1848 1849
  , CN.Branch 1849 (Eq (Plus (Num 0) (Id "new_top")) (Num 1)) 1850 1851
  , CN.Var 1850 "ref"
  , CN.Seq 1850 1851
  , CN.Seq 1850 1849
  , CN.Var 1851 "LOOP_FOOTER"
  , CN.Seq 1851 1852
  , CN.Branch 1852 (Eq (Id "lasti") (Num 1)) 1854 1856
  , CN.Var 1854 "frame_lasti"
  , CN.Seq 1854 1855
  , CN.Var 1855 "lasti"
  , CN.Seq 1855 1856
  , CN.Seq 1855 1857
  , CN.Var 1856 "NOP_1856"
  , CN.Seq 1856 1857
  , CN.Var 1857 "IF_ELSE_FOOTER"
  , CN.Var 1858 "exc"
  , CN.Seq 1858 1859
  , CN.Assign 1859 "next_instr" (Num 0)
  , CN.Seq 1859 1860
  , CN.Var 1860 "err"
  , CN.Seq 1860 1861
  , CN.Branch 1861 (Eq (Plus (Id "err") (Num 0)) (Num 1)) 1863 1863
  , CN.Seq 1862 1834
  , CN.Seq 1862 1864
  , CN.Var 1863 "NOP_1863"
  , CN.Seq 1863 1864
  , CN.Var 1864 "IF_ELSE_FOOTER"
  , CN.Assign 1865 "stack_pointer" (Num 0)
  , CN.Seq 1865 1866
  , CN.Var 1866 "word"
  , CN.Seq 1866 1867
  , CN.Assign 1867 "opcode" (Num 0)
  , CN.Seq 1867 1868
  , CN.Assign 1868 "oparg" (Num 0)
  , CN.Seq 1868 1869
  , CN.Branch 1869 (Eq (Num 0) (Num 1)) 1870 1873
  , CN.Var 1870 "word"
  , CN.Seq 1870 1871
  , CN.Assign 1871 "opcode" (Num 0)
  , CN.Seq 1871 1872
  , CN.Assign 1872 "oparg" (Num 0)
  , CN.Seq 1872 1873
  , CN.Seq 1872 1869
  , CN.Var 1873 "LOOP_FOOTER"
  , CN.Seq 1873 1874
  , CN.Seq 1873 35
  , CN.Var 1874 "__CLABEL_exit_unwind"
  , CN.Seq 1874 1875
  , CN.Var 1875 "dying"
  , CN.Seq 1875 1876
  , CN.Assign 1876 "frame" (Num 0)
  , CN.Seq 1876 1877
  , CN.Assign 1877 "undefed" (Num 0)
  , CN.Seq 1877 1878
  , CN.Branch 1878 (Eq (Plus (Num 0) (Id "FRAME_OWNED_BY_INTERPRETER")) (Num 1)) 1880 1882
  , CN.Assign 1880 "undefed" (Num 0)
  , CN.Seq 1880 1881
  , CN.Assign 1881 "return" (Num 0)
  , CN.Seq 1881 1882
  , CN.Seq 1881 1883
  , CN.Var 1882 "NOP_1882"
  , CN.Seq 1882 1883
  , CN.Var 1883 "IF_ELSE_FOOTER"
  , CN.Assign 1884 "next_instr" (Num 0)
  , CN.Seq 1884 1885
  , CN.Assign 1885 "stack_pointer" (Num 0)
  , CN.Seq 1885 1886
  , CN.Seq 1885 1817
  , CN.Var 1886 "__CLABEL_start_frame"
  , CN.Seq 1886 1887
  , CN.Var 1887 "too_deep"
  , CN.Seq 1887 1888
  , CN.Branch 1888 (Eq (Id "too_deep") (Num 1)) 1890 1890
  , CN.Seq 1889 1874
  , CN.Seq 1889 1891
  , CN.Var 1890 "NOP_1890"
  , CN.Seq 1890 1891
  , CN.Var 1891 "IF_ELSE_FOOTER"
  , CN.Assign 1892 "next_instr" (Num 0)
  , CN.Seq 1892 1893
  , CN.Assign 1893 "stack_pointer" (Num 0)
  , CN.Seq 1893 1894
  , CN.Var 1894 "word"
  , CN.Seq 1894 1895
  , CN.Assign 1895 "opcode" (Num 0)
  , CN.Seq 1895 1896
  , CN.Assign 1896 "oparg" (Num 0)
  , CN.Seq 1896 1897
  , CN.Branch 1897 (Eq (Num 0) (Num 1)) 1898 1901
  , CN.Var 1898 "word"
  , CN.Seq 1898 1899
  , CN.Assign 1899 "opcode" (Num 0)
  , CN.Seq 1899 1900
  , CN.Assign 1900 "oparg" (Num 0)
  , CN.Seq 1900 1901
  , CN.Seq 1900 1897
  , CN.Var 1901 "LOOP_FOOTER"
  , CN.Seq 1901 1902
  , CN.Seq 1901 35
  , CN.Var 1902 "__CLABEL_early_exit"
  , CN.Seq 1902 1903
  , CN.Var 1903 "NOP_1903"
  , CN.Var 1904 "dying"
  , CN.Seq 1904 1905
  , CN.Assign 1905 "frame" (Num 0)
  , CN.Seq 1905 1906
  , CN.Assign 1906 "undefed" (Num 0)
  , CN.Seq 1906 1907
  , CN.Assign 1907 "undefed" (Num 0)
  , CN.Seq 1907 1908
  , CN.Assign 1908 "return" (Num 0)
  , CN.Seq 1908 1909
  , CN.Seq 1908 1909
  , CN.Var 1909 "PROG_END"
  ]
