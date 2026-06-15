module ReducedProduct.CEval2K where

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
  , H.mkBranch 7 (H.Eq (H.Num 0) (H.Num 1)) 9 10
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
  , H.mkBranch 25 (H.Eq (H.Id "throwflag") (H.Num 1)) 27 33
  , H.mkBranch 27 (H.Eq (H.Num 0) (H.Num 1)) 29 29
  , H.mkSeq 28 1902
  , H.mkSeq 28 30
  , H.mkVar 29 "NOP_29"
  , H.mkSeq 29 30
  , H.mkVar 30 "IF_ELSE_FOOTER"
  , H.mkAssign 31 "next_instr" (H.Num 0)
  , H.mkSeq 31 32
  , H.mkAssign 32 "stack_pointer" (H.Num 0)
  , H.mkSeq 32 33
  , H.mkSeq 32 1817
  , H.mkSeq 32 34
  , H.mkVar 33 "NOP_33"
  , H.mkSeq 33 34
  , H.mkVar 34 "IF_ELSE_FOOTER"
  , H.mkSeq 34 1886
  , H.mkVar 35 "__CLABEL_dispatch_opcode"
  , H.mkSeq 35 36
  , H.mkBranch 36 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 38 87
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
  , H.mkBranch 51 (H.Eq (H.Num 0) (H.Num 1)) 53 56
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
  , H.mkBranch 59 (H.Eq (H.Num 0) (H.Num 1)) 60 61
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
  , H.mkBranch 66 (H.Eq (H.Plus (H.Id "res_o") (H.Num 0)) (H.Num 1)) 68 68
  , H.mkSeq 67 1817
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
  , H.mkBranch 82 (H.Eq (H.Num 0) (H.Num 1)) 83 86
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
  , H.mkBranch 87 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 89 129
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
  , H.mkBranch 100 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Plus (H.Id "PyFloat_Type") (H.Num 0))) (H.Num 0)) (H.Num 1)) 102 102
  , H.mkSeq 101 42
  , H.mkSeq 101 103
  , H.mkVar 102 "NOP_102"
  , H.mkSeq 102 103
  , H.mkVar 103 "IF_ELSE_FOOTER"
  , H.mkAssign 104 "left" (H.Num 0)
  , H.mkSeq 104 105
  , H.mkVar 105 "left_o"
  , H.mkSeq 105 106
  , H.mkBranch 106 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Plus (H.Id "PyFloat_Type") (H.Num 0))) (H.Num 0)) (H.Num 1)) 108 108
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
  , H.mkBranch 115 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 117 117
  , H.mkSeq 116 1813
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
  , H.mkBranch 124 (H.Eq (H.Num 0) (H.Num 1)) 125 128
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
  , H.mkBranch 129 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 131 170
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
  , H.mkBranch 142 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 144 144
  , H.mkSeq 143 42
  , H.mkSeq 143 145
  , H.mkVar 144 "NOP_144"
  , H.mkSeq 144 145
  , H.mkVar 145 "IF_ELSE_FOOTER"
  , H.mkAssign 146 "left" (H.Num 0)
  , H.mkSeq 146 147
  , H.mkVar 147 "left_o"
  , H.mkSeq 147 148
  , H.mkBranch 148 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 150 150
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
  , H.mkBranch 156 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 158 158
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
  , H.mkBranch 165 (H.Eq (H.Num 0) (H.Num 1)) 166 169
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
  , H.mkBranch 170 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 172 214
  , H.mkVar 172 "NOP_172"
  , H.mkVar 173 "__CLABEL_TARGET_BINARY_OP_ADD_UNICODE"
  , H.mkSeq 173 174
  , H.mkVar 174 "this_instr"
  , H.mkSeq 174 175
  , H.mkAssign 175 "undefed" (H.Num 0)
  , H.mkSeq 175 176
  , H.mkAssign 176 "next_instr" (H.Num 0)
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
  , H.mkAssign 182 "value" (H.Num 0)
  , H.mkSeq 182 183
  , H.mkVar 183 "value_o"
  , H.mkSeq 183 184
  , H.mkBranch 184 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Plus (H.Id "PyUnicode_Type") (H.Num 0))) (H.Num 0)) (H.Num 1)) 186 186
  , H.mkSeq 185 42
  , H.mkSeq 185 187
  , H.mkVar 186 "NOP_186"
  , H.mkSeq 186 187
  , H.mkVar 187 "IF_ELSE_FOOTER"
  , H.mkAssign 188 "nos" (H.Num 0)
  , H.mkSeq 188 189
  , H.mkVar 189 "o"
  , H.mkSeq 189 190
  , H.mkBranch 190 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Plus (H.Id "PyUnicode_Type") (H.Num 0))) (H.Num 0)) (H.Num 1)) 192 192
  , H.mkSeq 191 42
  , H.mkSeq 191 193
  , H.mkVar 192 "NOP_192"
  , H.mkSeq 192 193
  , H.mkVar 193 "IF_ELSE_FOOTER"
  , H.mkAssign 194 "right" (H.Num 0)
  , H.mkSeq 194 195
  , H.mkAssign 195 "left" (H.Num 0)
  , H.mkSeq 195 196
  , H.mkVar 196 "left_o"
  , H.mkSeq 196 197
  , H.mkVar 197 "right_o"
  , H.mkSeq 197 198
  , H.mkVar 198 "res_o"
  , H.mkSeq 198 199
  , H.mkBranch 199 (H.Eq (H.Plus (H.Id "res_o") (H.Num 0)) (H.Num 1)) 201 201
  , H.mkSeq 200 1813
  , H.mkSeq 200 202
  , H.mkVar 201 "NOP_201"
  , H.mkSeq 201 202
  , H.mkVar 202 "IF_ELSE_FOOTER"
  , H.mkAssign 203 "res" (H.Num 0)
  , H.mkSeq 203 204
  , H.mkAssign 204 "undefed" (H.Num 0)
  , H.mkSeq 204 205
  , H.mkAssign 205 "stack_pointer" (H.Num 0)
  , H.mkSeq 205 206
  , H.mkVar 206 "word"
  , H.mkSeq 206 207
  , H.mkAssign 207 "opcode" (H.Num 0)
  , H.mkSeq 207 208
  , H.mkAssign 208 "oparg" (H.Num 0)
  , H.mkSeq 208 209
  , H.mkBranch 209 (H.Eq (H.Num 0) (H.Num 1)) 210 213
  , H.mkVar 210 "word"
  , H.mkSeq 210 211
  , H.mkAssign 211 "opcode" (H.Num 0)
  , H.mkSeq 211 212
  , H.mkAssign 212 "oparg" (H.Num 0)
  , H.mkSeq 212 213
  , H.mkSeq 212 209
  , H.mkVar 213 "LOOP_FOOTER"
  , H.mkSeq 213 214
  , H.mkSeq 213 35
  , H.mkBranch 214 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 216 260
  , H.mkVar 216 "NOP_216"
  , H.mkVar 217 "__CLABEL_TARGET_BINARY_OP_EXTEND"
  , H.mkSeq 217 218
  , H.mkVar 218 "this_instr"
  , H.mkSeq 218 219
  , H.mkAssign 219 "undefed" (H.Num 0)
  , H.mkSeq 219 220
  , H.mkAssign 220 "next_instr" (H.Num 0)
  , H.mkSeq 220 221
  , H.mkVar 221 "left"
  , H.mkSeq 221 222
  , H.mkVar 222 "right"
  , H.mkSeq 222 223
  , H.mkVar 223 "res"
  , H.mkSeq 223 224
  , H.mkAssign 224 "right" (H.Num 0)
  , H.mkSeq 224 225
  , H.mkAssign 225 "left" (H.Num 0)
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
  , H.mkAssign 231 "stack_pointer" (H.Num 0)
  , H.mkSeq 231 232
  , H.mkBranch 232 (H.Eq (H.Plus (H.Id "res") (H.Num 0)) (H.Num 1)) 234 234
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
  , H.mkAssign 242 "right" (H.Num 0)
  , H.mkSeq 242 243
  , H.mkAssign 243 "undefed" (H.Num 0)
  , H.mkSeq 243 244
  , H.mkAssign 244 "tmp" (H.Num 0)
  , H.mkSeq 244 245
  , H.mkAssign 245 "left" (H.Num 0)
  , H.mkSeq 245 246
  , H.mkAssign 246 "undefed" (H.Num 0)
  , H.mkSeq 246 247
  , H.mkAssign 247 "stack_pointer" (H.Num 0)
  , H.mkSeq 247 248
  , H.mkAssign 248 "stack_pointer" (H.Num 0)
  , H.mkSeq 248 249
  , H.mkAssign 249 "res" (H.Num 0)
  , H.mkSeq 249 250
  , H.mkAssign 250 "undefed" (H.Num 0)
  , H.mkSeq 250 251
  , H.mkAssign 251 "stack_pointer" (H.Num 0)
  , H.mkSeq 251 252
  , H.mkVar 252 "word"
  , H.mkSeq 252 253
  , H.mkAssign 253 "opcode" (H.Num 0)
  , H.mkSeq 253 254
  , H.mkAssign 254 "oparg" (H.Num 0)
  , H.mkSeq 254 255
  , H.mkBranch 255 (H.Eq (H.Num 0) (H.Num 1)) 256 259
  , H.mkVar 256 "word"
  , H.mkSeq 256 257
  , H.mkAssign 257 "opcode" (H.Num 0)
  , H.mkSeq 257 258
  , H.mkAssign 258 "oparg" (H.Num 0)
  , H.mkSeq 258 259
  , H.mkSeq 258 255
  , H.mkVar 259 "LOOP_FOOTER"
  , H.mkSeq 259 260
  , H.mkSeq 259 35
  , H.mkBranch 260 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 262 354
  , H.mkVar 262 "NOP_262"
  , H.mkVar 263 "__CLABEL_TARGET_BINARY_OP_INPLACE_ADD_UNICODE"
  , H.mkSeq 263 264
  , H.mkVar 264 "this_instr"
  , H.mkSeq 264 265
  , H.mkAssign 265 "undefed" (H.Num 0)
  , H.mkSeq 265 266
  , H.mkAssign 266 "next_instr" (H.Num 0)
  , H.mkSeq 266 267
  , H.mkVar 267 "value"
  , H.mkSeq 267 268
  , H.mkVar 268 "nos"
  , H.mkSeq 268 269
  , H.mkVar 269 "left"
  , H.mkSeq 269 270
  , H.mkVar 270 "right"
  , H.mkSeq 270 271
  , H.mkAssign 271 "value" (H.Num 0)
  , H.mkSeq 271 272
  , H.mkVar 272 "value_o"
  , H.mkSeq 272 273
  , H.mkBranch 273 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Plus (H.Id "PyUnicode_Type") (H.Num 0))) (H.Num 0)) (H.Num 1)) 275 275
  , H.mkSeq 274 42
  , H.mkSeq 274 276
  , H.mkVar 275 "NOP_275"
  , H.mkSeq 275 276
  , H.mkVar 276 "IF_ELSE_FOOTER"
  , H.mkAssign 277 "nos" (H.Num 0)
  , H.mkSeq 277 278
  , H.mkVar 278 "o"
  , H.mkSeq 278 279
  , H.mkBranch 279 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Plus (H.Id "PyUnicode_Type") (H.Num 0))) (H.Num 0)) (H.Num 1)) 281 281
  , H.mkSeq 280 42
  , H.mkSeq 280 282
  , H.mkVar 281 "NOP_281"
  , H.mkSeq 281 282
  , H.mkVar 282 "IF_ELSE_FOOTER"
  , H.mkAssign 283 "right" (H.Num 0)
  , H.mkSeq 283 284
  , H.mkAssign 284 "left" (H.Num 0)
  , H.mkSeq 284 285
  , H.mkVar 285 "left_o"
  , H.mkSeq 285 286
  , H.mkVar 286 "next_oparg"
  , H.mkSeq 286 287
  , H.mkAssign 287 "next_oparg" (H.Num 0)
  , H.mkSeq 287 288
  , H.mkVar 288 "target_local"
  , H.mkSeq 288 289
  , H.mkBranch 289 (H.Eq (H.Plus (H.Num 0) (H.Id "left_o")) (H.Num 1)) 291 291
  , H.mkSeq 290 42
  , H.mkSeq 290 292
  , H.mkVar 291 "NOP_291"
  , H.mkSeq 291 292
  , H.mkVar 292 "IF_ELSE_FOOTER"
  , H.mkVar 293 "temp"
  , H.mkSeq 293 294
  , H.mkVar 294 "right_o"
  , H.mkSeq 294 295
  , H.mkAssign 295 "stack_pointer" (H.Num 0)
  , H.mkSeq 295 296
  , H.mkAssign 296 "stack_pointer" (H.Num 0)
  , H.mkSeq 296 297
  , H.mkAssign 297 "undefed" (H.Num 0)
  , H.mkSeq 297 298
  , H.mkVar 298 "op"
  , H.mkSeq 298 299
  , H.mkBranch 299 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Num 0)) (H.Num 0)) (H.Num 1)) 301 316
  , H.mkVar 301 "tracer"
  , H.mkSeq 301 302
  , H.mkBranch 302 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 304 305
  , H.mkVar 304 "data"
  , H.mkSeq 304 305
  , H.mkSeq 304 306
  , H.mkVar 305 "NOP_305"
  , H.mkSeq 305 306
  , H.mkVar 306 "IF_ELSE_FOOTER"
  , H.mkBranch 307 (H.Eq (H.Num 0) (H.Num 1)) 308 314
  , H.mkVar 308 "tracer"
  , H.mkSeq 308 309
  , H.mkBranch 309 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 311 312
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
  , H.mkBranch 318 (H.Eq (H.Num 0) (H.Num 1)) 319 339
  , H.mkVar 319 "op"
  , H.mkSeq 319 320
  , H.mkBranch 320 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Num 0)) (H.Num 0)) (H.Num 1)) 322 337
  , H.mkVar 322 "tracer"
  , H.mkSeq 322 323
  , H.mkBranch 323 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 325 326
  , H.mkVar 325 "data"
  , H.mkSeq 325 326
  , H.mkSeq 325 327
  , H.mkVar 326 "NOP_326"
  , H.mkSeq 326 327
  , H.mkVar 327 "IF_ELSE_FOOTER"
  , H.mkBranch 328 (H.Eq (H.Num 0) (H.Num 1)) 329 335
  , H.mkVar 329 "tracer"
  , H.mkSeq 329 330
  , H.mkBranch 330 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 332 333
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
  , H.mkAssign 340 "stack_pointer" (H.Num 0)
  , H.mkSeq 340 341
  , H.mkBranch 341 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 343 343
  , H.mkSeq 342 1817
  , H.mkSeq 342 344
  , H.mkVar 343 "NOP_343"
  , H.mkSeq 343 344
  , H.mkVar 344 "IF_ELSE_FOOTER"
  , H.mkAssign 345 "next_instr" (H.Num 0)
  , H.mkSeq 345 346
  , H.mkVar 346 "word"
  , H.mkSeq 346 347
  , H.mkAssign 347 "opcode" (H.Num 0)
  , H.mkSeq 347 348
  , H.mkAssign 348 "oparg" (H.Num 0)
  , H.mkSeq 348 349
  , H.mkBranch 349 (H.Eq (H.Num 0) (H.Num 1)) 350 353
  , H.mkVar 350 "word"
  , H.mkSeq 350 351
  , H.mkAssign 351 "opcode" (H.Num 0)
  , H.mkSeq 351 352
  , H.mkAssign 352 "oparg" (H.Num 0)
  , H.mkSeq 352 353
  , H.mkSeq 352 349
  , H.mkVar 353 "LOOP_FOOTER"
  , H.mkSeq 353 354
  , H.mkSeq 353 35
  , H.mkBranch 354 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 356 396
  , H.mkVar 356 "NOP_356"
  , H.mkVar 357 "__CLABEL_TARGET_BINARY_OP_MULTIPLY_FLOAT"
  , H.mkSeq 357 358
  , H.mkVar 358 "this_instr"
  , H.mkSeq 358 359
  , H.mkAssign 359 "undefed" (H.Num 0)
  , H.mkSeq 359 360
  , H.mkAssign 360 "next_instr" (H.Num 0)
  , H.mkSeq 360 361
  , H.mkVar 361 "value"
  , H.mkSeq 361 362
  , H.mkVar 362 "left"
  , H.mkSeq 362 363
  , H.mkVar 363 "right"
  , H.mkSeq 363 364
  , H.mkVar 364 "res"
  , H.mkSeq 364 365
  , H.mkAssign 365 "value" (H.Num 0)
  , H.mkSeq 365 366
  , H.mkVar 366 "value_o"
  , H.mkSeq 366 367
  , H.mkBranch 367 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Plus (H.Id "PyFloat_Type") (H.Num 0))) (H.Num 0)) (H.Num 1)) 369 369
  , H.mkSeq 368 42
  , H.mkSeq 368 370
  , H.mkVar 369 "NOP_369"
  , H.mkSeq 369 370
  , H.mkVar 370 "IF_ELSE_FOOTER"
  , H.mkAssign 371 "left" (H.Num 0)
  , H.mkSeq 371 372
  , H.mkVar 372 "left_o"
  , H.mkSeq 372 373
  , H.mkBranch 373 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Plus (H.Id "PyFloat_Type") (H.Num 0))) (H.Num 0)) (H.Num 1)) 375 375
  , H.mkSeq 374 42
  , H.mkSeq 374 376
  , H.mkVar 375 "NOP_375"
  , H.mkSeq 375 376
  , H.mkVar 376 "IF_ELSE_FOOTER"
  , H.mkAssign 377 "right" (H.Num 0)
  , H.mkSeq 377 378
  , H.mkVar 378 "left_o"
  , H.mkSeq 378 379
  , H.mkVar 379 "right_o"
  , H.mkSeq 379 380
  , H.mkVar 380 "dres"
  , H.mkSeq 380 381
  , H.mkAssign 381 "res" (H.Num 0)
  , H.mkSeq 381 382
  , H.mkBranch 382 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 384 384
  , H.mkSeq 383 1813
  , H.mkSeq 383 385
  , H.mkVar 384 "NOP_384"
  , H.mkSeq 384 385
  , H.mkVar 385 "IF_ELSE_FOOTER"
  , H.mkAssign 386 "undefed" (H.Num 0)
  , H.mkSeq 386 387
  , H.mkAssign 387 "stack_pointer" (H.Num 0)
  , H.mkSeq 387 388
  , H.mkVar 388 "word"
  , H.mkSeq 388 389
  , H.mkAssign 389 "opcode" (H.Num 0)
  , H.mkSeq 389 390
  , H.mkAssign 390 "oparg" (H.Num 0)
  , H.mkSeq 390 391
  , H.mkBranch 391 (H.Eq (H.Num 0) (H.Num 1)) 392 395
  , H.mkVar 392 "word"
  , H.mkSeq 392 393
  , H.mkAssign 393 "opcode" (H.Num 0)
  , H.mkSeq 393 394
  , H.mkAssign 394 "oparg" (H.Num 0)
  , H.mkSeq 394 395
  , H.mkSeq 394 391
  , H.mkVar 395 "LOOP_FOOTER"
  , H.mkSeq 395 396
  , H.mkSeq 395 35
  , H.mkBranch 396 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 398 437
  , H.mkVar 398 "NOP_398"
  , H.mkVar 399 "__CLABEL_TARGET_BINARY_OP_MULTIPLY_INT"
  , H.mkSeq 399 400
  , H.mkVar 400 "this_instr"
  , H.mkSeq 400 401
  , H.mkAssign 401 "undefed" (H.Num 0)
  , H.mkSeq 401 402
  , H.mkAssign 402 "next_instr" (H.Num 0)
  , H.mkSeq 402 403
  , H.mkVar 403 "value"
  , H.mkSeq 403 404
  , H.mkVar 404 "left"
  , H.mkSeq 404 405
  , H.mkVar 405 "right"
  , H.mkSeq 405 406
  , H.mkVar 406 "res"
  , H.mkSeq 406 407
  , H.mkAssign 407 "value" (H.Num 0)
  , H.mkSeq 407 408
  , H.mkVar 408 "value_o"
  , H.mkSeq 408 409
  , H.mkBranch 409 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 411 411
  , H.mkSeq 410 42
  , H.mkSeq 410 412
  , H.mkVar 411 "NOP_411"
  , H.mkSeq 411 412
  , H.mkVar 412 "IF_ELSE_FOOTER"
  , H.mkAssign 413 "left" (H.Num 0)
  , H.mkSeq 413 414
  , H.mkVar 414 "left_o"
  , H.mkSeq 414 415
  , H.mkBranch 415 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 417 417
  , H.mkSeq 416 42
  , H.mkSeq 416 418
  , H.mkVar 417 "NOP_417"
  , H.mkSeq 417 418
  , H.mkVar 418 "IF_ELSE_FOOTER"
  , H.mkAssign 419 "right" (H.Num 0)
  , H.mkSeq 419 420
  , H.mkVar 420 "left_o"
  , H.mkSeq 420 421
  , H.mkVar 421 "right_o"
  , H.mkSeq 421 422
  , H.mkAssign 422 "res" (H.Num 0)
  , H.mkSeq 422 423
  , H.mkBranch 423 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 425 425
  , H.mkSeq 424 42
  , H.mkSeq 424 426
  , H.mkVar 425 "NOP_425"
  , H.mkSeq 425 426
  , H.mkVar 426 "IF_ELSE_FOOTER"
  , H.mkAssign 427 "undefed" (H.Num 0)
  , H.mkSeq 427 428
  , H.mkAssign 428 "stack_pointer" (H.Num 0)
  , H.mkSeq 428 429
  , H.mkVar 429 "word"
  , H.mkSeq 429 430
  , H.mkAssign 430 "opcode" (H.Num 0)
  , H.mkSeq 430 431
  , H.mkAssign 431 "oparg" (H.Num 0)
  , H.mkSeq 431 432
  , H.mkBranch 432 (H.Eq (H.Num 0) (H.Num 1)) 433 436
  , H.mkVar 433 "word"
  , H.mkSeq 433 434
  , H.mkAssign 434 "opcode" (H.Num 0)
  , H.mkSeq 434 435
  , H.mkAssign 435 "oparg" (H.Num 0)
  , H.mkSeq 435 436
  , H.mkSeq 435 432
  , H.mkVar 436 "LOOP_FOOTER"
  , H.mkSeq 436 437
  , H.mkSeq 436 35
  , H.mkBranch 437 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 439 489
  , H.mkVar 439 "NOP_439"
  , H.mkVar 440 "__CLABEL_TARGET_BINARY_OP_SUBSCR_DICT"
  , H.mkSeq 440 441
  , H.mkVar 441 "this_instr"
  , H.mkSeq 441 442
  , H.mkAssign 442 "undefed" (H.Num 0)
  , H.mkSeq 442 443
  , H.mkAssign 443 "next_instr" (H.Num 0)
  , H.mkSeq 443 444
  , H.mkVar 444 "nos"
  , H.mkSeq 444 445
  , H.mkVar 445 "dict_st"
  , H.mkSeq 445 446
  , H.mkVar 446 "sub_st"
  , H.mkSeq 446 447
  , H.mkVar 447 "res"
  , H.mkSeq 447 448
  , H.mkAssign 448 "nos" (H.Num 0)
  , H.mkSeq 448 449
  , H.mkVar 449 "o"
  , H.mkSeq 449 450
  , H.mkBranch 450 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Plus (H.Id "PyDict_Type") (H.Num 0))) (H.Num 0)) (H.Num 1)) 452 452
  , H.mkSeq 451 42
  , H.mkSeq 451 453
  , H.mkVar 452 "NOP_452"
  , H.mkSeq 452 453
  , H.mkVar 453 "IF_ELSE_FOOTER"
  , H.mkAssign 454 "sub_st" (H.Num 0)
  , H.mkSeq 454 455
  , H.mkAssign 455 "dict_st" (H.Num 0)
  , H.mkSeq 455 456
  , H.mkVar 456 "sub"
  , H.mkSeq 456 457
  , H.mkVar 457 "dict"
  , H.mkSeq 457 458
  , H.mkVar 458 "res_o"
  , H.mkSeq 458 459
  , H.mkVar 459 "rc"
  , H.mkSeq 459 460
  , H.mkAssign 460 "stack_pointer" (H.Num 0)
  , H.mkSeq 460 461
  , H.mkBranch 461 (H.Eq (H.Plus (H.Id "rc") (H.Num 0)) (H.Num 1)) 463 464
  , H.mkAssign 463 "stack_pointer" (H.Num 0)
  , H.mkSeq 463 465
  , H.mkVar 464 "NOP_464"
  , H.mkSeq 464 465
  , H.mkVar 465 "IF_ELSE_FOOTER"
  , H.mkVar 466 "tmp"
  , H.mkSeq 466 467
  , H.mkAssign 467 "sub_st" (H.Num 0)
  , H.mkSeq 467 468
  , H.mkAssign 468 "undefed" (H.Num 0)
  , H.mkSeq 468 469
  , H.mkAssign 469 "tmp" (H.Num 0)
  , H.mkSeq 469 470
  , H.mkAssign 470 "dict_st" (H.Num 0)
  , H.mkSeq 470 471
  , H.mkAssign 471 "undefed" (H.Num 0)
  , H.mkSeq 471 472
  , H.mkAssign 472 "stack_pointer" (H.Num 0)
  , H.mkSeq 472 473
  , H.mkAssign 473 "stack_pointer" (H.Num 0)
  , H.mkSeq 473 474
  , H.mkBranch 474 (H.Eq (H.Plus (H.Id "rc") (H.Num 0)) (H.Num 1)) 476 476
  , H.mkSeq 475 1817
  , H.mkSeq 475 477
  , H.mkVar 476 "NOP_476"
  , H.mkSeq 476 477
  , H.mkVar 477 "IF_ELSE_FOOTER"
  , H.mkAssign 478 "res" (H.Num 0)
  , H.mkSeq 478 479
  , H.mkAssign 479 "undefed" (H.Num 0)
  , H.mkSeq 479 480
  , H.mkAssign 480 "stack_pointer" (H.Num 0)
  , H.mkSeq 480 481
  , H.mkVar 481 "word"
  , H.mkSeq 481 482
  , H.mkAssign 482 "opcode" (H.Num 0)
  , H.mkSeq 482 483
  , H.mkAssign 483 "oparg" (H.Num 0)
  , H.mkSeq 483 484
  , H.mkBranch 484 (H.Eq (H.Num 0) (H.Num 1)) 485 488
  , H.mkVar 485 "word"
  , H.mkSeq 485 486
  , H.mkAssign 486 "opcode" (H.Num 0)
  , H.mkSeq 486 487
  , H.mkAssign 487 "oparg" (H.Num 0)
  , H.mkSeq 487 488
  , H.mkSeq 487 484
  , H.mkVar 488 "LOOP_FOOTER"
  , H.mkSeq 488 489
  , H.mkSeq 488 35
  , H.mkBranch 489 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 491 549
  , H.mkVar 491 "NOP_491"
  , H.mkVar 492 "__CLABEL_TARGET_BINARY_OP_SUBSCR_GETITEM"
  , H.mkSeq 492 493
  , H.mkVar 493 "this_instr"
  , H.mkSeq 493 494
  , H.mkAssign 494 "undefed" (H.Num 0)
  , H.mkSeq 494 495
  , H.mkAssign 495 "next_instr" (H.Num 0)
  , H.mkSeq 495 496
  , H.mkVar 496 "container"
  , H.mkSeq 496 497
  , H.mkVar 497 "getitem"
  , H.mkSeq 497 498
  , H.mkVar 498 "sub"
  , H.mkSeq 498 499
  , H.mkVar 499 "new_frame"
  , H.mkSeq 499 500
  , H.mkBranch 500 (H.Eq (H.Num 0) (H.Num 1)) 502 502
  , H.mkSeq 501 42
  , H.mkSeq 501 503
  , H.mkVar 502 "NOP_502"
  , H.mkSeq 502 503
  , H.mkVar 503 "IF_ELSE_FOOTER"
  , H.mkAssign 504 "container" (H.Num 0)
  , H.mkSeq 504 505
  , H.mkVar 505 "tp"
  , H.mkSeq 505 506
  , H.mkBranch 506 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 508 508
  , H.mkSeq 507 42
  , H.mkSeq 507 509
  , H.mkVar 508 "NOP_508"
  , H.mkSeq 508 509
  , H.mkVar 509 "IF_ELSE_FOOTER"
  , H.mkVar 510 "ht"
  , H.mkSeq 510 511
  , H.mkVar 511 "getitem_o"
  , H.mkSeq 511 512
  , H.mkBranch 512 (H.Eq (H.Plus (H.Id "getitem_o") (H.Num 0)) (H.Num 1)) 514 514
  , H.mkSeq 513 42
  , H.mkSeq 513 515
  , H.mkVar 514 "NOP_514"
  , H.mkSeq 514 515
  , H.mkVar 515 "IF_ELSE_FOOTER"
  , H.mkVar 516 "cached_version"
  , H.mkSeq 516 517
  , H.mkBranch 517 (H.Eq (H.Plus (H.Num 0) (H.Id "cached_version")) (H.Num 1)) 519 519
  , H.mkSeq 518 42
  , H.mkSeq 518 520
  , H.mkVar 519 "NOP_519"
  , H.mkSeq 519 520
  , H.mkVar 520 "IF_ELSE_FOOTER"
  , H.mkVar 521 "code"
  , H.mkSeq 521 522
  , H.mkBranch 522 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 524 524
  , H.mkSeq 523 42
  , H.mkSeq 523 525
  , H.mkVar 524 "NOP_524"
  , H.mkSeq 524 525
  , H.mkVar 525 "IF_ELSE_FOOTER"
  , H.mkAssign 526 "getitem" (H.Num 0)
  , H.mkSeq 526 527
  , H.mkAssign 527 "sub" (H.Num 0)
  , H.mkSeq 527 528
  , H.mkVar 528 "pushed_frame"
  , H.mkSeq 528 529
  , H.mkAssign 529 "undefed" (H.Num 0)
  , H.mkSeq 529 530
  , H.mkAssign 530 "undefed" (H.Num 0)
  , H.mkSeq 530 531
  , H.mkAssign 531 "undefed" (H.Num 0)
  , H.mkSeq 531 532
  , H.mkAssign 532 "new_frame" (H.Num 0)
  , H.mkSeq 532 533
  , H.mkVar 533 "temp"
  , H.mkSeq 533 534
  , H.mkAssign 534 "stack_pointer" (H.Num 0)
  , H.mkSeq 534 535
  , H.mkAssign 535 "frame" (H.Num 0)
  , H.mkSeq 535 536
  , H.mkAssign 536 "stack_pointer" (H.Num 0)
  , H.mkSeq 536 537
  , H.mkAssign 537 "next_instr" (H.Num 0)
  , H.mkSeq 537 538
  , H.mkBranch 538 (H.Eq (H.Num 0) (H.Num 1)) 539 540
  , H.mkAssign 539 "next_instr" (H.Num 0)
  , H.mkSeq 539 538
  , H.mkVar 540 "LOOP_FOOTER"
  , H.mkSeq 540 541
  , H.mkVar 541 "word"
  , H.mkSeq 541 542
  , H.mkAssign 542 "opcode" (H.Num 0)
  , H.mkSeq 542 543
  , H.mkAssign 543 "oparg" (H.Num 0)
  , H.mkSeq 543 544
  , H.mkBranch 544 (H.Eq (H.Num 0) (H.Num 1)) 545 548
  , H.mkVar 545 "word"
  , H.mkSeq 545 546
  , H.mkAssign 546 "opcode" (H.Num 0)
  , H.mkSeq 546 547
  , H.mkAssign 547 "oparg" (H.Num 0)
  , H.mkSeq 547 548
  , H.mkSeq 547 544
  , H.mkVar 548 "LOOP_FOOTER"
  , H.mkSeq 548 549
  , H.mkSeq 548 35
  , H.mkBranch 549 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 551 604
  , H.mkVar 551 "NOP_551"
  , H.mkVar 552 "__CLABEL_TARGET_BINARY_OP_SUBSCR_LIST_INT"
  , H.mkSeq 552 553
  , H.mkVar 553 "this_instr"
  , H.mkSeq 553 554
  , H.mkAssign 554 "undefed" (H.Num 0)
  , H.mkSeq 554 555
  , H.mkAssign 555 "next_instr" (H.Num 0)
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
  , H.mkAssign 561 "value" (H.Num 0)
  , H.mkSeq 561 562
  , H.mkVar 562 "value_o"
  , H.mkSeq 562 563
  , H.mkBranch 563 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 565 565
  , H.mkSeq 564 42
  , H.mkSeq 564 566
  , H.mkVar 565 "NOP_565"
  , H.mkSeq 565 566
  , H.mkVar 566 "IF_ELSE_FOOTER"
  , H.mkAssign 567 "nos" (H.Num 0)
  , H.mkSeq 567 568
  , H.mkVar 568 "o"
  , H.mkSeq 568 569
  , H.mkBranch 569 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Plus (H.Id "PyList_Type") (H.Num 0))) (H.Num 0)) (H.Num 1)) 571 571
  , H.mkSeq 570 42
  , H.mkSeq 570 572
  , H.mkVar 571 "NOP_571"
  , H.mkSeq 571 572
  , H.mkVar 572 "IF_ELSE_FOOTER"
  , H.mkAssign 573 "sub_st" (H.Num 0)
  , H.mkSeq 573 574
  , H.mkAssign 574 "list_st" (H.Num 0)
  , H.mkSeq 574 575
  , H.mkVar 575 "sub"
  , H.mkSeq 575 576
  , H.mkVar 576 "list"
  , H.mkSeq 576 577
  , H.mkBranch 577 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 579 579
  , H.mkSeq 578 42
  , H.mkSeq 578 580
  , H.mkVar 579 "NOP_579"
  , H.mkSeq 579 580
  , H.mkVar 580 "IF_ELSE_FOOTER"
  , H.mkVar 581 "index"
  , H.mkSeq 581 582
  , H.mkBranch 582 (H.Eq (H.Plus (H.Id "index") (H.Num 0)) (H.Num 1)) 584 584
  , H.mkSeq 583 42
  , H.mkSeq 583 585
  , H.mkVar 584 "NOP_584"
  , H.mkSeq 584 585
  , H.mkVar 585 "IF_ELSE_FOOTER"
  , H.mkVar 586 "res_o"
  , H.mkSeq 586 587
  , H.mkAssign 587 "res" (H.Num 0)
  , H.mkSeq 587 588
  , H.mkVar 588 "tmp"
  , H.mkSeq 588 589
  , H.mkAssign 589 "list_st" (H.Num 0)
  , H.mkSeq 589 590
  , H.mkAssign 590 "undefed" (H.Num 0)
  , H.mkSeq 590 591
  , H.mkAssign 591 "tmp" (H.Num 0)
  , H.mkSeq 591 592
  , H.mkAssign 592 "sub_st" (H.Num 0)
  , H.mkSeq 592 593
  , H.mkAssign 593 "undefed" (H.Num 0)
  , H.mkSeq 593 594
  , H.mkAssign 594 "stack_pointer" (H.Num 0)
  , H.mkSeq 594 595
  , H.mkAssign 595 "stack_pointer" (H.Num 0)
  , H.mkSeq 595 596
  , H.mkVar 596 "word"
  , H.mkSeq 596 597
  , H.mkAssign 597 "opcode" (H.Num 0)
  , H.mkSeq 597 598
  , H.mkAssign 598 "oparg" (H.Num 0)
  , H.mkSeq 598 599
  , H.mkBranch 599 (H.Eq (H.Num 0) (H.Num 1)) 600 603
  , H.mkVar 600 "word"
  , H.mkSeq 600 601
  , H.mkAssign 601 "opcode" (H.Num 0)
  , H.mkSeq 601 602
  , H.mkAssign 602 "oparg" (H.Num 0)
  , H.mkSeq 602 603
  , H.mkSeq 602 599
  , H.mkVar 603 "LOOP_FOOTER"
  , H.mkSeq 603 604
  , H.mkSeq 603 35
  , H.mkBranch 604 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 606 657
  , H.mkVar 606 "NOP_606"
  , H.mkVar 607 "__CLABEL_TARGET_BINARY_OP_SUBSCR_LIST_SLICE"
  , H.mkSeq 607 608
  , H.mkVar 608 "this_instr"
  , H.mkSeq 608 609
  , H.mkAssign 609 "undefed" (H.Num 0)
  , H.mkSeq 609 610
  , H.mkAssign 610 "next_instr" (H.Num 0)
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
  , H.mkAssign 616 "tos" (H.Num 0)
  , H.mkSeq 616 617
  , H.mkVar 617 "o"
  , H.mkSeq 617 618
  , H.mkBranch 618 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Plus (H.Id "PySlice_Type") (H.Num 0))) (H.Num 0)) (H.Num 1)) 620 620
  , H.mkSeq 619 42
  , H.mkSeq 619 621
  , H.mkVar 620 "NOP_620"
  , H.mkSeq 620 621
  , H.mkVar 621 "IF_ELSE_FOOTER"
  , H.mkAssign 622 "nos" (H.Num 0)
  , H.mkSeq 622 623
  , H.mkVar 623 "o"
  , H.mkSeq 623 624
  , H.mkBranch 624 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Plus (H.Id "PyList_Type") (H.Num 0))) (H.Num 0)) (H.Num 1)) 626 626
  , H.mkSeq 625 42
  , H.mkSeq 625 627
  , H.mkVar 626 "NOP_626"
  , H.mkSeq 626 627
  , H.mkVar 627 "IF_ELSE_FOOTER"
  , H.mkAssign 628 "sub_st" (H.Num 0)
  , H.mkSeq 628 629
  , H.mkAssign 629 "list_st" (H.Num 0)
  , H.mkSeq 629 630
  , H.mkVar 630 "sub"
  , H.mkSeq 630 631
  , H.mkVar 631 "list"
  , H.mkSeq 631 632
  , H.mkVar 632 "res_o"
  , H.mkSeq 632 633
  , H.mkAssign 633 "stack_pointer" (H.Num 0)
  , H.mkSeq 633 634
  , H.mkVar 634 "tmp"
  , H.mkSeq 634 635
  , H.mkAssign 635 "sub_st" (H.Num 0)
  , H.mkSeq 635 636
  , H.mkAssign 636 "undefed" (H.Num 0)
  , H.mkSeq 636 637
  , H.mkAssign 637 "tmp" (H.Num 0)
  , H.mkSeq 637 638
  , H.mkAssign 638 "list_st" (H.Num 0)
  , H.mkSeq 638 639
  , H.mkAssign 639 "undefed" (H.Num 0)
  , H.mkSeq 639 640
  , H.mkAssign 640 "stack_pointer" (H.Num 0)
  , H.mkSeq 640 641
  , H.mkAssign 641 "stack_pointer" (H.Num 0)
  , H.mkSeq 641 642
  , H.mkBranch 642 (H.Eq (H.Plus (H.Id "res_o") (H.Num 0)) (H.Num 1)) 644 644
  , H.mkSeq 643 1817
  , H.mkSeq 643 645
  , H.mkVar 644 "NOP_644"
  , H.mkSeq 644 645
  , H.mkVar 645 "IF_ELSE_FOOTER"
  , H.mkAssign 646 "res" (H.Num 0)
  , H.mkSeq 646 647
  , H.mkAssign 647 "undefed" (H.Num 0)
  , H.mkSeq 647 648
  , H.mkAssign 648 "stack_pointer" (H.Num 0)
  , H.mkSeq 648 649
  , H.mkVar 649 "word"
  , H.mkSeq 649 650
  , H.mkAssign 650 "opcode" (H.Num 0)
  , H.mkSeq 650 651
  , H.mkAssign 651 "oparg" (H.Num 0)
  , H.mkSeq 651 652
  , H.mkBranch 652 (H.Eq (H.Num 0) (H.Num 1)) 653 656
  , H.mkVar 653 "word"
  , H.mkSeq 653 654
  , H.mkAssign 654 "opcode" (H.Num 0)
  , H.mkSeq 654 655
  , H.mkAssign 655 "oparg" (H.Num 0)
  , H.mkSeq 655 656
  , H.mkSeq 655 652
  , H.mkVar 656 "LOOP_FOOTER"
  , H.mkSeq 656 657
  , H.mkSeq 656 35
  , H.mkBranch 657 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 659 713
  , H.mkVar 659 "NOP_659"
  , H.mkVar 660 "__CLABEL_TARGET_BINARY_OP_SUBSCR_STR_INT"
  , H.mkSeq 660 661
  , H.mkVar 661 "this_instr"
  , H.mkSeq 661 662
  , H.mkAssign 662 "undefed" (H.Num 0)
  , H.mkSeq 662 663
  , H.mkAssign 663 "next_instr" (H.Num 0)
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
  , H.mkAssign 669 "value" (H.Num 0)
  , H.mkSeq 669 670
  , H.mkVar 670 "value_o"
  , H.mkSeq 670 671
  , H.mkBranch 671 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 673 673
  , H.mkSeq 672 42
  , H.mkSeq 672 674
  , H.mkVar 673 "NOP_673"
  , H.mkSeq 673 674
  , H.mkVar 674 "IF_ELSE_FOOTER"
  , H.mkAssign 675 "nos" (H.Num 0)
  , H.mkSeq 675 676
  , H.mkVar 676 "o"
  , H.mkSeq 676 677
  , H.mkBranch 677 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Plus (H.Id "PyUnicode_Type") (H.Num 0))) (H.Num 0)) (H.Num 1)) 679 679
  , H.mkSeq 678 42
  , H.mkSeq 678 680
  , H.mkVar 679 "NOP_679"
  , H.mkSeq 679 680
  , H.mkVar 680 "IF_ELSE_FOOTER"
  , H.mkAssign 681 "sub_st" (H.Num 0)
  , H.mkSeq 681 682
  , H.mkAssign 682 "str_st" (H.Num 0)
  , H.mkSeq 682 683
  , H.mkVar 683 "sub"
  , H.mkSeq 683 684
  , H.mkVar 684 "str"
  , H.mkSeq 684 685
  , H.mkBranch 685 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 687 687
  , H.mkSeq 686 42
  , H.mkSeq 686 688
  , H.mkVar 687 "NOP_687"
  , H.mkSeq 687 688
  , H.mkVar 688 "IF_ELSE_FOOTER"
  , H.mkVar 689 "index"
  , H.mkSeq 689 690
  , H.mkBranch 690 (H.Eq (H.Plus (H.Num 0) (H.Id "index")) (H.Num 1)) 692 692
  , H.mkSeq 691 42
  , H.mkSeq 691 693
  , H.mkVar 692 "NOP_692"
  , H.mkSeq 692 693
  , H.mkVar 693 "IF_ELSE_FOOTER"
  , H.mkVar 694 "c"
  , H.mkSeq 694 695
  , H.mkBranch 695 (H.Eq (H.Plus (H.Plus (H.Plus (H.Num 0) (H.Num 0)) (H.Plus (H.Num 0) (H.Num 0))) (H.Id "c")) (H.Num 1)) 697 697
  , H.mkSeq 696 42
  , H.mkSeq 696 698
  , H.mkVar 697 "NOP_697"
  , H.mkSeq 697 698
  , H.mkVar 698 "IF_ELSE_FOOTER"
  , H.mkVar 699 "res_o"
  , H.mkSeq 699 700
  , H.mkAssign 700 "stack_pointer" (H.Num 0)
  , H.mkSeq 700 701
  , H.mkAssign 701 "stack_pointer" (H.Num 0)
  , H.mkSeq 701 702
  , H.mkAssign 702 "res" (H.Num 0)
  , H.mkSeq 702 703
  , H.mkAssign 703 "undefed" (H.Num 0)
  , H.mkSeq 703 704
  , H.mkAssign 704 "stack_pointer" (H.Num 0)
  , H.mkSeq 704 705
  , H.mkVar 705 "word"
  , H.mkSeq 705 706
  , H.mkAssign 706 "opcode" (H.Num 0)
  , H.mkSeq 706 707
  , H.mkAssign 707 "oparg" (H.Num 0)
  , H.mkSeq 707 708
  , H.mkBranch 708 (H.Eq (H.Num 0) (H.Num 1)) 709 712
  , H.mkVar 709 "word"
  , H.mkSeq 709 710
  , H.mkAssign 710 "opcode" (H.Num 0)
  , H.mkSeq 710 711
  , H.mkAssign 711 "oparg" (H.Num 0)
  , H.mkSeq 711 712
  , H.mkSeq 711 708
  , H.mkVar 712 "LOOP_FOOTER"
  , H.mkSeq 712 713
  , H.mkSeq 712 35
  , H.mkBranch 713 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 715 765
  , H.mkVar 715 "NOP_715"
  , H.mkVar 716 "__CLABEL_TARGET_BINARY_OP_SUBSCR_TUPLE_INT"
  , H.mkSeq 716 717
  , H.mkVar 717 "this_instr"
  , H.mkSeq 717 718
  , H.mkAssign 718 "undefed" (H.Num 0)
  , H.mkSeq 718 719
  , H.mkAssign 719 "next_instr" (H.Num 0)
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
  , H.mkAssign 725 "value" (H.Num 0)
  , H.mkSeq 725 726
  , H.mkVar 726 "value_o"
  , H.mkSeq 726 727
  , H.mkBranch 727 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 729 729
  , H.mkSeq 728 42
  , H.mkSeq 728 730
  , H.mkVar 729 "NOP_729"
  , H.mkSeq 729 730
  , H.mkVar 730 "IF_ELSE_FOOTER"
  , H.mkAssign 731 "nos" (H.Num 0)
  , H.mkSeq 731 732
  , H.mkVar 732 "o"
  , H.mkSeq 732 733
  , H.mkBranch 733 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Plus (H.Id "PyTuple_Type") (H.Num 0))) (H.Num 0)) (H.Num 1)) 735 735
  , H.mkSeq 734 42
  , H.mkSeq 734 736
  , H.mkVar 735 "NOP_735"
  , H.mkSeq 735 736
  , H.mkVar 736 "IF_ELSE_FOOTER"
  , H.mkAssign 737 "sub_st" (H.Num 0)
  , H.mkSeq 737 738
  , H.mkAssign 738 "tuple_st" (H.Num 0)
  , H.mkSeq 738 739
  , H.mkVar 739 "sub"
  , H.mkSeq 739 740
  , H.mkVar 740 "tuple"
  , H.mkSeq 740 741
  , H.mkBranch 741 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 743 743
  , H.mkSeq 742 42
  , H.mkSeq 742 744
  , H.mkVar 743 "NOP_743"
  , H.mkSeq 743 744
  , H.mkVar 744 "IF_ELSE_FOOTER"
  , H.mkVar 745 "index"
  , H.mkSeq 745 746
  , H.mkBranch 746 (H.Eq (H.Plus (H.Id "index") (H.Num 0)) (H.Num 1)) 748 748
  , H.mkSeq 747 42
  , H.mkSeq 747 749
  , H.mkVar 748 "NOP_748"
  , H.mkSeq 748 749
  , H.mkVar 749 "IF_ELSE_FOOTER"
  , H.mkVar 750 "res_o"
  , H.mkSeq 750 751
  , H.mkAssign 751 "res" (H.Num 0)
  , H.mkSeq 751 752
  , H.mkAssign 752 "stack_pointer" (H.Num 0)
  , H.mkSeq 752 753
  , H.mkVar 753 "tmp"
  , H.mkSeq 753 754
  , H.mkAssign 754 "tuple_st" (H.Num 0)
  , H.mkSeq 754 755
  , H.mkAssign 755 "undefed" (H.Num 0)
  , H.mkSeq 755 756
  , H.mkAssign 756 "stack_pointer" (H.Num 0)
  , H.mkSeq 756 757
  , H.mkVar 757 "word"
  , H.mkSeq 757 758
  , H.mkAssign 758 "opcode" (H.Num 0)
  , H.mkSeq 758 759
  , H.mkAssign 759 "oparg" (H.Num 0)
  , H.mkSeq 759 760
  , H.mkBranch 760 (H.Eq (H.Num 0) (H.Num 1)) 761 764
  , H.mkVar 761 "word"
  , H.mkSeq 761 762
  , H.mkAssign 762 "opcode" (H.Num 0)
  , H.mkSeq 762 763
  , H.mkAssign 763 "oparg" (H.Num 0)
  , H.mkSeq 763 764
  , H.mkSeq 763 760
  , H.mkVar 764 "LOOP_FOOTER"
  , H.mkSeq 764 765
  , H.mkSeq 764 35
  , H.mkBranch 765 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 767 807
  , H.mkVar 767 "NOP_767"
  , H.mkVar 768 "__CLABEL_TARGET_BINARY_OP_SUBTRACT_FLOAT"
  , H.mkSeq 768 769
  , H.mkVar 769 "this_instr"
  , H.mkSeq 769 770
  , H.mkAssign 770 "undefed" (H.Num 0)
  , H.mkSeq 770 771
  , H.mkAssign 771 "next_instr" (H.Num 0)
  , H.mkSeq 771 772
  , H.mkVar 772 "value"
  , H.mkSeq 772 773
  , H.mkVar 773 "left"
  , H.mkSeq 773 774
  , H.mkVar 774 "right"
  , H.mkSeq 774 775
  , H.mkVar 775 "res"
  , H.mkSeq 775 776
  , H.mkAssign 776 "value" (H.Num 0)
  , H.mkSeq 776 777
  , H.mkVar 777 "value_o"
  , H.mkSeq 777 778
  , H.mkBranch 778 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Plus (H.Id "PyFloat_Type") (H.Num 0))) (H.Num 0)) (H.Num 1)) 780 780
  , H.mkSeq 779 42
  , H.mkSeq 779 781
  , H.mkVar 780 "NOP_780"
  , H.mkSeq 780 781
  , H.mkVar 781 "IF_ELSE_FOOTER"
  , H.mkAssign 782 "left" (H.Num 0)
  , H.mkSeq 782 783
  , H.mkVar 783 "left_o"
  , H.mkSeq 783 784
  , H.mkBranch 784 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Plus (H.Id "PyFloat_Type") (H.Num 0))) (H.Num 0)) (H.Num 1)) 786 786
  , H.mkSeq 785 42
  , H.mkSeq 785 787
  , H.mkVar 786 "NOP_786"
  , H.mkSeq 786 787
  , H.mkVar 787 "IF_ELSE_FOOTER"
  , H.mkAssign 788 "right" (H.Num 0)
  , H.mkSeq 788 789
  , H.mkVar 789 "left_o"
  , H.mkSeq 789 790
  , H.mkVar 790 "right_o"
  , H.mkSeq 790 791
  , H.mkVar 791 "dres"
  , H.mkSeq 791 792
  , H.mkAssign 792 "res" (H.Num 0)
  , H.mkSeq 792 793
  , H.mkBranch 793 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 795 795
  , H.mkSeq 794 1813
  , H.mkSeq 794 796
  , H.mkVar 795 "NOP_795"
  , H.mkSeq 795 796
  , H.mkVar 796 "IF_ELSE_FOOTER"
  , H.mkAssign 797 "undefed" (H.Num 0)
  , H.mkSeq 797 798
  , H.mkAssign 798 "stack_pointer" (H.Num 0)
  , H.mkSeq 798 799
  , H.mkVar 799 "word"
  , H.mkSeq 799 800
  , H.mkAssign 800 "opcode" (H.Num 0)
  , H.mkSeq 800 801
  , H.mkAssign 801 "oparg" (H.Num 0)
  , H.mkSeq 801 802
  , H.mkBranch 802 (H.Eq (H.Num 0) (H.Num 1)) 803 806
  , H.mkVar 803 "word"
  , H.mkSeq 803 804
  , H.mkAssign 804 "opcode" (H.Num 0)
  , H.mkSeq 804 805
  , H.mkAssign 805 "oparg" (H.Num 0)
  , H.mkSeq 805 806
  , H.mkSeq 805 802
  , H.mkVar 806 "LOOP_FOOTER"
  , H.mkSeq 806 807
  , H.mkSeq 806 35
  , H.mkBranch 807 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 809 848
  , H.mkVar 809 "NOP_809"
  , H.mkVar 810 "__CLABEL_TARGET_BINARY_OP_SUBTRACT_INT"
  , H.mkSeq 810 811
  , H.mkVar 811 "this_instr"
  , H.mkSeq 811 812
  , H.mkAssign 812 "undefed" (H.Num 0)
  , H.mkSeq 812 813
  , H.mkAssign 813 "next_instr" (H.Num 0)
  , H.mkSeq 813 814
  , H.mkVar 814 "value"
  , H.mkSeq 814 815
  , H.mkVar 815 "left"
  , H.mkSeq 815 816
  , H.mkVar 816 "right"
  , H.mkSeq 816 817
  , H.mkVar 817 "res"
  , H.mkSeq 817 818
  , H.mkAssign 818 "value" (H.Num 0)
  , H.mkSeq 818 819
  , H.mkVar 819 "value_o"
  , H.mkSeq 819 820
  , H.mkBranch 820 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 822 822
  , H.mkSeq 821 42
  , H.mkSeq 821 823
  , H.mkVar 822 "NOP_822"
  , H.mkSeq 822 823
  , H.mkVar 823 "IF_ELSE_FOOTER"
  , H.mkAssign 824 "left" (H.Num 0)
  , H.mkSeq 824 825
  , H.mkVar 825 "left_o"
  , H.mkSeq 825 826
  , H.mkBranch 826 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 828 828
  , H.mkSeq 827 42
  , H.mkSeq 827 829
  , H.mkVar 828 "NOP_828"
  , H.mkSeq 828 829
  , H.mkVar 829 "IF_ELSE_FOOTER"
  , H.mkAssign 830 "right" (H.Num 0)
  , H.mkSeq 830 831
  , H.mkVar 831 "left_o"
  , H.mkSeq 831 832
  , H.mkVar 832 "right_o"
  , H.mkSeq 832 833
  , H.mkAssign 833 "res" (H.Num 0)
  , H.mkSeq 833 834
  , H.mkBranch 834 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 836 836
  , H.mkSeq 835 42
  , H.mkSeq 835 837
  , H.mkVar 836 "NOP_836"
  , H.mkSeq 836 837
  , H.mkVar 837 "IF_ELSE_FOOTER"
  , H.mkAssign 838 "undefed" (H.Num 0)
  , H.mkSeq 838 839
  , H.mkAssign 839 "stack_pointer" (H.Num 0)
  , H.mkSeq 839 840
  , H.mkVar 840 "word"
  , H.mkSeq 840 841
  , H.mkAssign 841 "opcode" (H.Num 0)
  , H.mkSeq 841 842
  , H.mkAssign 842 "oparg" (H.Num 0)
  , H.mkSeq 842 843
  , H.mkBranch 843 (H.Eq (H.Num 0) (H.Num 1)) 844 847
  , H.mkVar 844 "word"
  , H.mkSeq 844 845
  , H.mkAssign 845 "opcode" (H.Num 0)
  , H.mkSeq 845 846
  , H.mkAssign 846 "oparg" (H.Num 0)
  , H.mkSeq 846 847
  , H.mkSeq 846 843
  , H.mkVar 847 "LOOP_FOOTER"
  , H.mkSeq 847 848
  , H.mkSeq 847 35
  , H.mkBranch 848 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 850 931
  , H.mkVar 850 "NOP_850"
  , H.mkVar 851 "__CLABEL_TARGET_BINARY_SLICE"
  , H.mkSeq 851 852
  , H.mkAssign 852 "undefed" (H.Num 0)
  , H.mkSeq 852 853
  , H.mkAssign 853 "next_instr" (H.Num 0)
  , H.mkSeq 853 854
  , H.mkVar 854 "container"
  , H.mkSeq 854 855
  , H.mkVar 855 "start"
  , H.mkSeq 855 856
  , H.mkVar 856 "stop"
  , H.mkSeq 856 857
  , H.mkVar 857 "res"
  , H.mkSeq 857 858
  , H.mkAssign 858 "stop" (H.Num 0)
  , H.mkSeq 858 859
  , H.mkAssign 859 "start" (H.Num 0)
  , H.mkSeq 859 860
  , H.mkAssign 860 "container" (H.Num 0)
  , H.mkSeq 860 861
  , H.mkVar 861 "slice"
  , H.mkSeq 861 862
  , H.mkAssign 862 "stack_pointer" (H.Num 0)
  , H.mkSeq 862 863
  , H.mkVar 863 "res_o"
  , H.mkSeq 863 864
  , H.mkBranch 864 (H.Eq (H.Plus (H.Id "slice") (H.Num 0)) (H.Num 1)) 866 867
  , H.mkAssign 866 "res_o" (H.Num 0)
  , H.mkSeq 866 913
  , H.mkAssign 867 "stack_pointer" (H.Num 0)
  , H.mkSeq 867 868
  , H.mkAssign 868 "res_o" (H.Num 0)
  , H.mkSeq 868 869
  , H.mkVar 869 "op"
  , H.mkSeq 869 870
  , H.mkBranch 870 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Num 0)) (H.Num 0)) (H.Num 1)) 872 887
  , H.mkVar 872 "tracer"
  , H.mkSeq 872 873
  , H.mkBranch 873 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 875 876
  , H.mkVar 875 "data"
  , H.mkSeq 875 876
  , H.mkSeq 875 877
  , H.mkVar 876 "NOP_876"
  , H.mkSeq 876 877
  , H.mkVar 877 "IF_ELSE_FOOTER"
  , H.mkBranch 878 (H.Eq (H.Num 0) (H.Num 1)) 879 885
  , H.mkVar 879 "tracer"
  , H.mkSeq 879 880
  , H.mkBranch 880 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 882 883
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
  , H.mkBranch 889 (H.Eq (H.Num 0) (H.Num 1)) 890 910
  , H.mkVar 890 "op"
  , H.mkSeq 890 891
  , H.mkBranch 891 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Num 0)) (H.Num 0)) (H.Num 1)) 893 908
  , H.mkVar 893 "tracer"
  , H.mkSeq 893 894
  , H.mkBranch 894 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 896 897
  , H.mkVar 896 "data"
  , H.mkSeq 896 897
  , H.mkSeq 896 898
  , H.mkVar 897 "NOP_897"
  , H.mkSeq 897 898
  , H.mkVar 898 "IF_ELSE_FOOTER"
  , H.mkBranch 899 (H.Eq (H.Num 0) (H.Num 1)) 900 906
  , H.mkVar 900 "tracer"
  , H.mkSeq 900 901
  , H.mkBranch 901 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 903 904
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
  , H.mkAssign 911 "stack_pointer" (H.Num 0)
  , H.mkSeq 911 912
  , H.mkAssign 912 "stack_pointer" (H.Num 0)
  , H.mkSeq 912 913
  , H.mkVar 913 "IF_ELSE_FOOTER"
  , H.mkAssign 914 "stack_pointer" (H.Num 0)
  , H.mkSeq 914 915
  , H.mkAssign 915 "stack_pointer" (H.Num 0)
  , H.mkSeq 915 916
  , H.mkBranch 916 (H.Eq (H.Plus (H.Id "res_o") (H.Num 0)) (H.Num 1)) 918 918
  , H.mkSeq 917 1817
  , H.mkSeq 917 919
  , H.mkVar 918 "NOP_918"
  , H.mkSeq 918 919
  , H.mkVar 919 "IF_ELSE_FOOTER"
  , H.mkAssign 920 "res" (H.Num 0)
  , H.mkSeq 920 921
  , H.mkAssign 921 "undefed" (H.Num 0)
  , H.mkSeq 921 922
  , H.mkAssign 922 "stack_pointer" (H.Num 0)
  , H.mkSeq 922 923
  , H.mkVar 923 "word"
  , H.mkSeq 923 924
  , H.mkAssign 924 "opcode" (H.Num 0)
  , H.mkSeq 924 925
  , H.mkAssign 925 "oparg" (H.Num 0)
  , H.mkSeq 925 926
  , H.mkBranch 926 (H.Eq (H.Num 0) (H.Num 1)) 927 930
  , H.mkVar 927 "word"
  , H.mkSeq 927 928
  , H.mkAssign 928 "opcode" (H.Num 0)
  , H.mkSeq 928 929
  , H.mkAssign 929 "oparg" (H.Num 0)
  , H.mkSeq 929 930
  , H.mkSeq 929 926
  , H.mkVar 930 "LOOP_FOOTER"
  , H.mkSeq 930 931
  , H.mkSeq 930 35
  , H.mkBranch 931 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 933 980
  , H.mkVar 933 "NOP_933"
  , H.mkVar 934 "__CLABEL_TARGET_BUILD_INTERPOLATION"
  , H.mkSeq 934 935
  , H.mkAssign 935 "undefed" (H.Num 0)
  , H.mkSeq 935 936
  , H.mkAssign 936 "next_instr" (H.Num 0)
  , H.mkSeq 936 937
  , H.mkVar 937 "value"
  , H.mkSeq 937 938
  , H.mkVar 938 "str"
  , H.mkSeq 938 939
  , H.mkVar 939 "format"
  , H.mkSeq 939 940
  , H.mkVar 940 "interpolation"
  , H.mkSeq 940 941
  , H.mkAssign 941 "format" (H.Num 0)
  , H.mkSeq 941 942
  , H.mkAssign 942 "str" (H.Num 0)
  , H.mkSeq 942 943
  , H.mkAssign 943 "value" (H.Num 0)
  , H.mkSeq 943 944
  , H.mkVar 944 "value_o"
  , H.mkSeq 944 945
  , H.mkVar 945 "str_o"
  , H.mkSeq 945 946
  , H.mkVar 946 "conversion"
  , H.mkSeq 946 947
  , H.mkVar 947 "format_o"
  , H.mkSeq 947 948
  , H.mkBranch 948 (H.Eq (H.Plus (H.Id "oparg") (H.Num 0)) (H.Num 1)) 950 951
  , H.mkAssign 950 "format_o" (H.Num 0)
  , H.mkSeq 950 952
  , H.mkAssign 951 "format_o" (H.Num 0)
  , H.mkSeq 951 952
  , H.mkVar 952 "IF_ELSE_FOOTER"
  , H.mkVar 953 "interpolation_o"
  , H.mkSeq 953 954
  , H.mkAssign 954 "stack_pointer" (H.Num 0)
  , H.mkSeq 954 955
  , H.mkBranch 955 (H.Eq (H.Plus (H.Id "oparg") (H.Num 0)) (H.Num 1)) 957 959
  , H.mkAssign 957 "stack_pointer" (H.Num 0)
  , H.mkSeq 957 958
  , H.mkAssign 958 "stack_pointer" (H.Num 0)
  , H.mkSeq 958 959
  , H.mkSeq 958 960
  , H.mkAssign 959 "stack_pointer" (H.Num 0)
  , H.mkSeq 959 960
  , H.mkVar 960 "IF_ELSE_FOOTER"
  , H.mkAssign 961 "stack_pointer" (H.Num 0)
  , H.mkSeq 961 962
  , H.mkAssign 962 "stack_pointer" (H.Num 0)
  , H.mkSeq 962 963
  , H.mkAssign 963 "stack_pointer" (H.Num 0)
  , H.mkSeq 963 964
  , H.mkAssign 964 "stack_pointer" (H.Num 0)
  , H.mkSeq 964 965
  , H.mkBranch 965 (H.Eq (H.Plus (H.Id "interpolation_o") (H.Num 0)) (H.Num 1)) 967 967
  , H.mkSeq 966 1817
  , H.mkSeq 966 968
  , H.mkVar 967 "NOP_967"
  , H.mkSeq 967 968
  , H.mkVar 968 "IF_ELSE_FOOTER"
  , H.mkAssign 969 "interpolation" (H.Num 0)
  , H.mkSeq 969 970
  , H.mkAssign 970 "undefed" (H.Num 0)
  , H.mkSeq 970 971
  , H.mkAssign 971 "stack_pointer" (H.Num 0)
  , H.mkSeq 971 972
  , H.mkVar 972 "word"
  , H.mkSeq 972 973
  , H.mkAssign 973 "opcode" (H.Num 0)
  , H.mkSeq 973 974
  , H.mkAssign 974 "oparg" (H.Num 0)
  , H.mkSeq 974 975
  , H.mkBranch 975 (H.Eq (H.Num 0) (H.Num 1)) 976 979
  , H.mkVar 976 "word"
  , H.mkSeq 976 977
  , H.mkAssign 977 "opcode" (H.Num 0)
  , H.mkSeq 977 978
  , H.mkAssign 978 "oparg" (H.Num 0)
  , H.mkSeq 978 979
  , H.mkSeq 978 975
  , H.mkVar 979 "LOOP_FOOTER"
  , H.mkSeq 979 980
  , H.mkSeq 979 35
  , H.mkBranch 980 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 982 1006
  , H.mkVar 982 "NOP_982"
  , H.mkVar 983 "__CLABEL_TARGET_BUILD_LIST"
  , H.mkSeq 983 984
  , H.mkAssign 984 "undefed" (H.Num 0)
  , H.mkSeq 984 985
  , H.mkAssign 985 "next_instr" (H.Num 0)
  , H.mkSeq 985 986
  , H.mkVar 986 "values"
  , H.mkSeq 986 987
  , H.mkVar 987 "list"
  , H.mkSeq 987 988
  , H.mkAssign 988 "values" (H.Num 0)
  , H.mkSeq 988 989
  , H.mkVar 989 "list_o"
  , H.mkSeq 989 990
  , H.mkAssign 990 "stack_pointer" (H.Num 0)
  , H.mkSeq 990 991
  , H.mkBranch 991 (H.Eq (H.Plus (H.Id "list_o") (H.Num 0)) (H.Num 1)) 993 993
  , H.mkSeq 992 1817
  , H.mkSeq 992 994
  , H.mkVar 993 "NOP_993"
  , H.mkSeq 993 994
  , H.mkVar 994 "IF_ELSE_FOOTER"
  , H.mkAssign 995 "list" (H.Num 0)
  , H.mkSeq 995 996
  , H.mkAssign 996 "undefed" (H.Num 0)
  , H.mkSeq 996 997
  , H.mkAssign 997 "stack_pointer" (H.Num 0)
  , H.mkSeq 997 998
  , H.mkVar 998 "word"
  , H.mkSeq 998 999
  , H.mkAssign 999 "opcode" (H.Num 0)
  , H.mkSeq 999 1000
  , H.mkAssign 1000 "oparg" (H.Num 0)
  , H.mkSeq 1000 1001
  , H.mkBranch 1001 (H.Eq (H.Num 0) (H.Num 1)) 1002 1005
  , H.mkVar 1002 "word"
  , H.mkSeq 1002 1003
  , H.mkAssign 1003 "opcode" (H.Num 0)
  , H.mkSeq 1003 1004
  , H.mkAssign 1004 "oparg" (H.Num 0)
  , H.mkSeq 1004 1005
  , H.mkSeq 1004 1001
  , H.mkVar 1005 "LOOP_FOOTER"
  , H.mkSeq 1005 1006
  , H.mkSeq 1005 35
  , H.mkBranch 1006 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 1008 1054
  , H.mkVar 1008 "NOP_1008"
  , H.mkVar 1009 "__CLABEL_TARGET_BUILD_MAP"
  , H.mkSeq 1009 1010
  , H.mkAssign 1010 "undefed" (H.Num 0)
  , H.mkSeq 1010 1011
  , H.mkAssign 1011 "next_instr" (H.Num 0)
  , H.mkSeq 1011 1012
  , H.mkVar 1012 "values"
  , H.mkSeq 1012 1013
  , H.mkVar 1013 "map"
  , H.mkSeq 1013 1014
  , H.mkAssign 1014 "values" (H.Num 0)
  , H.mkSeq 1014 1015
  , H.mkVar 1015 "values_o_temp"
  , H.mkSeq 1015 1016
  , H.mkVar 1016 "values_o"
  , H.mkSeq 1016 1017
  , H.mkBranch 1017 (H.Eq (H.Plus (H.Id "values_o") (H.Num 0)) (H.Num 1)) 1019 1027
  , H.mkVar 1019 "tmp"
  , H.mkSeq 1019 1020
  , H.mkVar 1020 "_i"
  , H.mkSeq 1020 1021
  , H.mkBranch 1021 (H.Eq (H.Plus (H.Plus (H.Id "_i") (H.Num 0)) (H.Num 0)) (H.Num 1)) 1022 1024
  , H.mkAssign 1022 "tmp" (H.Num 0)
  , H.mkSeq 1022 1023
  , H.mkAssign 1023 "undefed" (H.Num 0)
  , H.mkSeq 1023 1024
  , H.mkSeq 1023 1021
  , H.mkVar 1024 "LOOP_FOOTER"
  , H.mkSeq 1024 1025
  , H.mkAssign 1025 "stack_pointer" (H.Num 0)
  , H.mkSeq 1025 1026
  , H.mkAssign 1026 "stack_pointer" (H.Num 0)
  , H.mkSeq 1026 1027
  , H.mkSeq 1026 1817
  , H.mkSeq 1026 1028
  , H.mkVar 1027 "NOP_1027"
  , H.mkSeq 1027 1028
  , H.mkVar 1028 "IF_ELSE_FOOTER"
  , H.mkVar 1029 "map_o"
  , H.mkSeq 1029 1030
  , H.mkAssign 1030 "stack_pointer" (H.Num 0)
  , H.mkSeq 1030 1031
  , H.mkVar 1031 "tmp"
  , H.mkSeq 1031 1032
  , H.mkVar 1032 "_i"
  , H.mkSeq 1032 1033
  , H.mkBranch 1033 (H.Eq (H.Plus (H.Plus (H.Id "_i") (H.Num 0)) (H.Num 0)) (H.Num 1)) 1034 1036
  , H.mkAssign 1034 "tmp" (H.Num 0)
  , H.mkSeq 1034 1035
  , H.mkAssign 1035 "undefed" (H.Num 0)
  , H.mkSeq 1035 1036
  , H.mkSeq 1035 1033
  , H.mkVar 1036 "LOOP_FOOTER"
  , H.mkSeq 1036 1037
  , H.mkAssign 1037 "stack_pointer" (H.Num 0)
  , H.mkSeq 1037 1038
  , H.mkAssign 1038 "stack_pointer" (H.Num 0)
  , H.mkSeq 1038 1039
  , H.mkBranch 1039 (H.Eq (H.Plus (H.Id "map_o") (H.Num 0)) (H.Num 1)) 1041 1041
  , H.mkSeq 1040 1817
  , H.mkSeq 1040 1042
  , H.mkVar 1041 "NOP_1041"
  , H.mkSeq 1041 1042
  , H.mkVar 1042 "IF_ELSE_FOOTER"
  , H.mkAssign 1043 "map" (H.Num 0)
  , H.mkSeq 1043 1044
  , H.mkAssign 1044 "undefed" (H.Num 0)
  , H.mkSeq 1044 1045
  , H.mkAssign 1045 "stack_pointer" (H.Num 0)
  , H.mkSeq 1045 1046
  , H.mkVar 1046 "word"
  , H.mkSeq 1046 1047
  , H.mkAssign 1047 "opcode" (H.Num 0)
  , H.mkSeq 1047 1048
  , H.mkAssign 1048 "oparg" (H.Num 0)
  , H.mkSeq 1048 1049
  , H.mkBranch 1049 (H.Eq (H.Num 0) (H.Num 1)) 1050 1053
  , H.mkVar 1050 "word"
  , H.mkSeq 1050 1051
  , H.mkAssign 1051 "opcode" (H.Num 0)
  , H.mkSeq 1051 1052
  , H.mkAssign 1052 "oparg" (H.Num 0)
  , H.mkSeq 1052 1053
  , H.mkSeq 1052 1049
  , H.mkVar 1053 "LOOP_FOOTER"
  , H.mkSeq 1053 1054
  , H.mkSeq 1053 35
  , H.mkBranch 1054 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 1056 1148
  , H.mkVar 1056 "NOP_1056"
  , H.mkVar 1057 "__CLABEL_TARGET_BUILD_SET"
  , H.mkSeq 1057 1058
  , H.mkAssign 1058 "undefed" (H.Num 0)
  , H.mkSeq 1058 1059
  , H.mkAssign 1059 "next_instr" (H.Num 0)
  , H.mkSeq 1059 1060
  , H.mkVar 1060 "values"
  , H.mkSeq 1060 1061
  , H.mkVar 1061 "set"
  , H.mkSeq 1061 1062
  , H.mkAssign 1062 "values" (H.Num 0)
  , H.mkSeq 1062 1063
  , H.mkVar 1063 "set_o"
  , H.mkSeq 1063 1064
  , H.mkAssign 1064 "stack_pointer" (H.Num 0)
  , H.mkSeq 1064 1065
  , H.mkBranch 1065 (H.Eq (H.Plus (H.Id "set_o") (H.Num 0)) (H.Num 1)) 1067 1075
  , H.mkVar 1067 "tmp"
  , H.mkSeq 1067 1068
  , H.mkVar 1068 "_i"
  , H.mkSeq 1068 1069
  , H.mkBranch 1069 (H.Eq (H.Plus (H.Plus (H.Id "_i") (H.Num 0)) (H.Num 0)) (H.Num 1)) 1070 1072
  , H.mkAssign 1070 "tmp" (H.Num 0)
  , H.mkSeq 1070 1071
  , H.mkAssign 1071 "undefed" (H.Num 0)
  , H.mkSeq 1071 1072
  , H.mkSeq 1071 1069
  , H.mkVar 1072 "LOOP_FOOTER"
  , H.mkSeq 1072 1073
  , H.mkAssign 1073 "stack_pointer" (H.Num 0)
  , H.mkSeq 1073 1074
  , H.mkAssign 1074 "stack_pointer" (H.Num 0)
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
  , H.mkBranch 1079 (H.Eq (H.Plus (H.Id "i") (H.Id "oparg")) (H.Num 1)) 1080 1088
  , H.mkVar 1080 "value"
  , H.mkSeq 1080 1081
  , H.mkAssign 1081 "undefed" (H.Num 0)
  , H.mkSeq 1081 1082
  , H.mkBranch 1082 (H.Eq (H.Plus (H.Id "err") (H.Num 0)) (H.Num 1)) 1084 1086
  , H.mkAssign 1084 "err" (H.Num 0)
  , H.mkSeq 1084 1085
  , H.mkAssign 1085 "stack_pointer" (H.Num 0)
  , H.mkSeq 1085 1086
  , H.mkSeq 1085 1087
  , H.mkAssign 1086 "stack_pointer" (H.Num 0)
  , H.mkSeq 1086 1087
  , H.mkVar 1087 "IF_ELSE_FOOTER"
  , H.mkSeq 1087 1079
  , H.mkVar 1088 "LOOP_FOOTER"
  , H.mkSeq 1088 1089
  , H.mkBranch 1089 (H.Eq (H.Id "err") (H.Num 1)) 1091 1135
  , H.mkAssign 1091 "stack_pointer" (H.Num 0)
  , H.mkSeq 1091 1092
  , H.mkVar 1092 "op"
  , H.mkSeq 1092 1093
  , H.mkBranch 1093 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Num 0)) (H.Num 0)) (H.Num 1)) 1095 1110
  , H.mkVar 1095 "tracer"
  , H.mkSeq 1095 1096
  , H.mkBranch 1096 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 1098 1099
  , H.mkVar 1098 "data"
  , H.mkSeq 1098 1099
  , H.mkSeq 1098 1100
  , H.mkVar 1099 "NOP_1099"
  , H.mkSeq 1099 1100
  , H.mkVar 1100 "IF_ELSE_FOOTER"
  , H.mkBranch 1101 (H.Eq (H.Num 0) (H.Num 1)) 1102 1108
  , H.mkVar 1102 "tracer"
  , H.mkSeq 1102 1103
  , H.mkBranch 1103 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 1105 1106
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
  , H.mkBranch 1112 (H.Eq (H.Num 0) (H.Num 1)) 1113 1133
  , H.mkVar 1113 "op"
  , H.mkSeq 1113 1114
  , H.mkBranch 1114 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Num 0)) (H.Num 0)) (H.Num 1)) 1116 1131
  , H.mkVar 1116 "tracer"
  , H.mkSeq 1116 1117
  , H.mkBranch 1117 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 1119 1120
  , H.mkVar 1119 "data"
  , H.mkSeq 1119 1120
  , H.mkSeq 1119 1121
  , H.mkVar 1120 "NOP_1120"
  , H.mkSeq 1120 1121
  , H.mkVar 1121 "IF_ELSE_FOOTER"
  , H.mkBranch 1122 (H.Eq (H.Num 0) (H.Num 1)) 1123 1129
  , H.mkVar 1123 "tracer"
  , H.mkSeq 1123 1124
  , H.mkBranch 1124 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 1126 1127
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
  , H.mkAssign 1134 "stack_pointer" (H.Num 0)
  , H.mkSeq 1134 1135
  , H.mkSeq 1134 1817
  , H.mkSeq 1134 1136
  , H.mkVar 1135 "NOP_1135"
  , H.mkSeq 1135 1136
  , H.mkVar 1136 "IF_ELSE_FOOTER"
  , H.mkAssign 1137 "set" (H.Num 0)
  , H.mkSeq 1137 1138
  , H.mkAssign 1138 "undefed" (H.Num 0)
  , H.mkSeq 1138 1139
  , H.mkAssign 1139 "stack_pointer" (H.Num 0)
  , H.mkSeq 1139 1140
  , H.mkVar 1140 "word"
  , H.mkSeq 1140 1141
  , H.mkAssign 1141 "opcode" (H.Num 0)
  , H.mkSeq 1141 1142
  , H.mkAssign 1142 "oparg" (H.Num 0)
  , H.mkSeq 1142 1143
  , H.mkBranch 1143 (H.Eq (H.Num 0) (H.Num 1)) 1144 1147
  , H.mkVar 1144 "word"
  , H.mkSeq 1144 1145
  , H.mkAssign 1145 "opcode" (H.Num 0)
  , H.mkSeq 1145 1146
  , H.mkAssign 1146 "oparg" (H.Num 0)
  , H.mkSeq 1146 1147
  , H.mkSeq 1146 1143
  , H.mkVar 1147 "LOOP_FOOTER"
  , H.mkSeq 1147 1148
  , H.mkSeq 1147 35
  , H.mkBranch 1148 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 1150 1184
  , H.mkVar 1150 "NOP_1150"
  , H.mkVar 1151 "__CLABEL_TARGET_BUILD_SLICE"
  , H.mkSeq 1151 1152
  , H.mkAssign 1152 "undefed" (H.Num 0)
  , H.mkSeq 1152 1153
  , H.mkAssign 1153 "next_instr" (H.Num 0)
  , H.mkSeq 1153 1154
  , H.mkVar 1154 "args"
  , H.mkSeq 1154 1155
  , H.mkVar 1155 "slice"
  , H.mkSeq 1155 1156
  , H.mkAssign 1156 "args" (H.Num 0)
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
  , H.mkBranch 1163 (H.Eq (H.Plus (H.Plus (H.Id "_i") (H.Num 0)) (H.Num 0)) (H.Num 1)) 1164 1166
  , H.mkAssign 1164 "tmp" (H.Num 0)
  , H.mkSeq 1164 1165
  , H.mkAssign 1165 "undefed" (H.Num 0)
  , H.mkSeq 1165 1166
  , H.mkSeq 1165 1163
  , H.mkVar 1166 "LOOP_FOOTER"
  , H.mkSeq 1166 1167
  , H.mkAssign 1167 "stack_pointer" (H.Num 0)
  , H.mkSeq 1167 1168
  , H.mkAssign 1168 "stack_pointer" (H.Num 0)
  , H.mkSeq 1168 1169
  , H.mkBranch 1169 (H.Eq (H.Plus (H.Id "slice_o") (H.Num 0)) (H.Num 1)) 1171 1171
  , H.mkSeq 1170 1817
  , H.mkSeq 1170 1172
  , H.mkVar 1171 "NOP_1171"
  , H.mkSeq 1171 1172
  , H.mkVar 1172 "IF_ELSE_FOOTER"
  , H.mkAssign 1173 "slice" (H.Num 0)
  , H.mkSeq 1173 1174
  , H.mkAssign 1174 "undefed" (H.Num 0)
  , H.mkSeq 1174 1175
  , H.mkAssign 1175 "stack_pointer" (H.Num 0)
  , H.mkSeq 1175 1176
  , H.mkVar 1176 "word"
  , H.mkSeq 1176 1177
  , H.mkAssign 1177 "opcode" (H.Num 0)
  , H.mkSeq 1177 1178
  , H.mkAssign 1178 "oparg" (H.Num 0)
  , H.mkSeq 1178 1179
  , H.mkBranch 1179 (H.Eq (H.Num 0) (H.Num 1)) 1180 1183
  , H.mkVar 1180 "word"
  , H.mkSeq 1180 1181
  , H.mkAssign 1181 "opcode" (H.Num 0)
  , H.mkSeq 1181 1182
  , H.mkAssign 1182 "oparg" (H.Num 0)
  , H.mkSeq 1182 1183
  , H.mkSeq 1182 1179
  , H.mkVar 1183 "LOOP_FOOTER"
  , H.mkSeq 1183 1184
  , H.mkSeq 1183 35
  , H.mkBranch 1184 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 1186 1231
  , H.mkVar 1186 "NOP_1186"
  , H.mkVar 1187 "__CLABEL_TARGET_BUILD_STRING"
  , H.mkSeq 1187 1188
  , H.mkAssign 1188 "undefed" (H.Num 0)
  , H.mkSeq 1188 1189
  , H.mkAssign 1189 "next_instr" (H.Num 0)
  , H.mkSeq 1189 1190
  , H.mkVar 1190 "pieces"
  , H.mkSeq 1190 1191
  , H.mkVar 1191 "str"
  , H.mkSeq 1191 1192
  , H.mkAssign 1192 "pieces" (H.Num 0)
  , H.mkSeq 1192 1193
  , H.mkVar 1193 "pieces_o_temp"
  , H.mkSeq 1193 1194
  , H.mkVar 1194 "pieces_o"
  , H.mkSeq 1194 1195
  , H.mkBranch 1195 (H.Eq (H.Plus (H.Id "pieces_o") (H.Num 0)) (H.Num 1)) 1197 1205
  , H.mkVar 1197 "tmp"
  , H.mkSeq 1197 1198
  , H.mkVar 1198 "_i"
  , H.mkSeq 1198 1199
  , H.mkBranch 1199 (H.Eq (H.Plus (H.Plus (H.Id "_i") (H.Num 0)) (H.Num 0)) (H.Num 1)) 1200 1202
  , H.mkAssign 1200 "tmp" (H.Num 0)
  , H.mkSeq 1200 1201
  , H.mkAssign 1201 "undefed" (H.Num 0)
  , H.mkSeq 1201 1202
  , H.mkSeq 1201 1199
  , H.mkVar 1202 "LOOP_FOOTER"
  , H.mkSeq 1202 1203
  , H.mkAssign 1203 "stack_pointer" (H.Num 0)
  , H.mkSeq 1203 1204
  , H.mkAssign 1204 "stack_pointer" (H.Num 0)
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
  , H.mkBranch 1210 (H.Eq (H.Plus (H.Plus (H.Id "_i") (H.Num 0)) (H.Num 0)) (H.Num 1)) 1211 1213
  , H.mkAssign 1211 "tmp" (H.Num 0)
  , H.mkSeq 1211 1212
  , H.mkAssign 1212 "undefed" (H.Num 0)
  , H.mkSeq 1212 1213
  , H.mkSeq 1212 1210
  , H.mkVar 1213 "LOOP_FOOTER"
  , H.mkSeq 1213 1214
  , H.mkAssign 1214 "stack_pointer" (H.Num 0)
  , H.mkSeq 1214 1215
  , H.mkAssign 1215 "stack_pointer" (H.Num 0)
  , H.mkSeq 1215 1216
  , H.mkBranch 1216 (H.Eq (H.Plus (H.Id "str_o") (H.Num 0)) (H.Num 1)) 1218 1218
  , H.mkSeq 1217 1817
  , H.mkSeq 1217 1219
  , H.mkVar 1218 "NOP_1218"
  , H.mkSeq 1218 1219
  , H.mkVar 1219 "IF_ELSE_FOOTER"
  , H.mkAssign 1220 "str" (H.Num 0)
  , H.mkSeq 1220 1221
  , H.mkAssign 1221 "undefed" (H.Num 0)
  , H.mkSeq 1221 1222
  , H.mkAssign 1222 "stack_pointer" (H.Num 0)
  , H.mkSeq 1222 1223
  , H.mkVar 1223 "word"
  , H.mkSeq 1223 1224
  , H.mkAssign 1224 "opcode" (H.Num 0)
  , H.mkSeq 1224 1225
  , H.mkAssign 1225 "oparg" (H.Num 0)
  , H.mkSeq 1225 1226
  , H.mkBranch 1226 (H.Eq (H.Num 0) (H.Num 1)) 1227 1230
  , H.mkVar 1227 "word"
  , H.mkSeq 1227 1228
  , H.mkAssign 1228 "opcode" (H.Num 0)
  , H.mkSeq 1228 1229
  , H.mkAssign 1229 "oparg" (H.Num 0)
  , H.mkSeq 1229 1230
  , H.mkSeq 1229 1226
  , H.mkVar 1230 "LOOP_FOOTER"
  , H.mkSeq 1230 1231
  , H.mkSeq 1230 35
  , H.mkBranch 1231 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 1233 1265
  , H.mkVar 1233 "NOP_1233"
  , H.mkVar 1234 "__CLABEL_TARGET_BUILD_TEMPLATE"
  , H.mkSeq 1234 1235
  , H.mkAssign 1235 "undefed" (H.Num 0)
  , H.mkSeq 1235 1236
  , H.mkAssign 1236 "next_instr" (H.Num 0)
  , H.mkSeq 1236 1237
  , H.mkVar 1237 "strings"
  , H.mkSeq 1237 1238
  , H.mkVar 1238 "interpolations"
  , H.mkSeq 1238 1239
  , H.mkVar 1239 "template"
  , H.mkSeq 1239 1240
  , H.mkAssign 1240 "interpolations" (H.Num 0)
  , H.mkSeq 1240 1241
  , H.mkAssign 1241 "strings" (H.Num 0)
  , H.mkSeq 1241 1242
  , H.mkVar 1242 "strings_o"
  , H.mkSeq 1242 1243
  , H.mkVar 1243 "interpolations_o"
  , H.mkSeq 1243 1244
  , H.mkVar 1244 "template_o"
  , H.mkSeq 1244 1245
  , H.mkAssign 1245 "stack_pointer" (H.Num 0)
  , H.mkSeq 1245 1246
  , H.mkAssign 1246 "stack_pointer" (H.Num 0)
  , H.mkSeq 1246 1247
  , H.mkAssign 1247 "stack_pointer" (H.Num 0)
  , H.mkSeq 1247 1248
  , H.mkAssign 1248 "stack_pointer" (H.Num 0)
  , H.mkSeq 1248 1249
  , H.mkAssign 1249 "stack_pointer" (H.Num 0)
  , H.mkSeq 1249 1250
  , H.mkBranch 1250 (H.Eq (H.Plus (H.Id "template_o") (H.Num 0)) (H.Num 1)) 1252 1252
  , H.mkSeq 1251 1817
  , H.mkSeq 1251 1253
  , H.mkVar 1252 "NOP_1252"
  , H.mkSeq 1252 1253
  , H.mkVar 1253 "IF_ELSE_FOOTER"
  , H.mkAssign 1254 "template" (H.Num 0)
  , H.mkSeq 1254 1255
  , H.mkAssign 1255 "undefed" (H.Num 0)
  , H.mkSeq 1255 1256
  , H.mkAssign 1256 "stack_pointer" (H.Num 0)
  , H.mkSeq 1256 1257
  , H.mkVar 1257 "word"
  , H.mkSeq 1257 1258
  , H.mkAssign 1258 "opcode" (H.Num 0)
  , H.mkSeq 1258 1259
  , H.mkAssign 1259 "oparg" (H.Num 0)
  , H.mkSeq 1259 1260
  , H.mkBranch 1260 (H.Eq (H.Num 0) (H.Num 1)) 1261 1264
  , H.mkVar 1261 "word"
  , H.mkSeq 1261 1262
  , H.mkAssign 1262 "opcode" (H.Num 0)
  , H.mkSeq 1262 1263
  , H.mkAssign 1263 "oparg" (H.Num 0)
  , H.mkSeq 1263 1264
  , H.mkSeq 1263 1260
  , H.mkVar 1264 "LOOP_FOOTER"
  , H.mkSeq 1264 1265
  , H.mkSeq 1264 35
  , H.mkBranch 1265 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 1267 1290
  , H.mkVar 1267 "NOP_1267"
  , H.mkVar 1268 "__CLABEL_TARGET_BUILD_TUPLE"
  , H.mkSeq 1268 1269
  , H.mkAssign 1269 "undefed" (H.Num 0)
  , H.mkSeq 1269 1270
  , H.mkAssign 1270 "next_instr" (H.Num 0)
  , H.mkSeq 1270 1271
  , H.mkVar 1271 "values"
  , H.mkSeq 1271 1272
  , H.mkVar 1272 "tup"
  , H.mkSeq 1272 1273
  , H.mkAssign 1273 "values" (H.Num 0)
  , H.mkSeq 1273 1274
  , H.mkVar 1274 "tup_o"
  , H.mkSeq 1274 1275
  , H.mkBranch 1275 (H.Eq (H.Plus (H.Id "tup_o") (H.Num 0)) (H.Num 1)) 1277 1277
  , H.mkSeq 1276 1817
  , H.mkSeq 1276 1278
  , H.mkVar 1277 "NOP_1277"
  , H.mkSeq 1277 1278
  , H.mkVar 1278 "IF_ELSE_FOOTER"
  , H.mkAssign 1279 "tup" (H.Num 0)
  , H.mkSeq 1279 1280
  , H.mkAssign 1280 "undefed" (H.Num 0)
  , H.mkSeq 1280 1281
  , H.mkAssign 1281 "stack_pointer" (H.Num 0)
  , H.mkSeq 1281 1282
  , H.mkVar 1282 "word"
  , H.mkSeq 1282 1283
  , H.mkAssign 1283 "opcode" (H.Num 0)
  , H.mkSeq 1283 1284
  , H.mkAssign 1284 "oparg" (H.Num 0)
  , H.mkSeq 1284 1285
  , H.mkBranch 1285 (H.Eq (H.Num 0) (H.Num 1)) 1286 1289
  , H.mkVar 1286 "word"
  , H.mkSeq 1286 1287
  , H.mkAssign 1287 "opcode" (H.Num 0)
  , H.mkSeq 1287 1288
  , H.mkAssign 1288 "oparg" (H.Num 0)
  , H.mkSeq 1288 1289
  , H.mkSeq 1288 1285
  , H.mkVar 1289 "LOOP_FOOTER"
  , H.mkSeq 1289 1290
  , H.mkSeq 1289 35
  , H.mkBranch 1290 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 1292 1304
  , H.mkVar 1292 "NOP_1292"
  , H.mkVar 1293 "__CLABEL_TARGET_CACHE"
  , H.mkSeq 1293 1294
  , H.mkAssign 1294 "undefed" (H.Num 0)
  , H.mkSeq 1294 1295
  , H.mkAssign 1295 "next_instr" (H.Num 0)
  , H.mkSeq 1295 1296
  , H.mkVar 1296 "word"
  , H.mkSeq 1296 1297
  , H.mkAssign 1297 "opcode" (H.Num 0)
  , H.mkSeq 1297 1298
  , H.mkAssign 1298 "oparg" (H.Num 0)
  , H.mkSeq 1298 1299
  , H.mkBranch 1299 (H.Eq (H.Num 0) (H.Num 1)) 1300 1303
  , H.mkVar 1300 "word"
  , H.mkSeq 1300 1301
  , H.mkAssign 1301 "opcode" (H.Num 0)
  , H.mkSeq 1301 1302
  , H.mkAssign 1302 "oparg" (H.Num 0)
  , H.mkSeq 1302 1303
  , H.mkSeq 1302 1299
  , H.mkVar 1303 "LOOP_FOOTER"
  , H.mkSeq 1303 1304
  , H.mkSeq 1303 35
  , H.mkBranch 1304 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 1306 1551
  , H.mkVar 1306 "NOP_1306"
  , H.mkVar 1307 "__CLABEL_TARGET_CALL"
  , H.mkSeq 1307 1308
  , H.mkAssign 1308 "undefed" (H.Num 0)
  , H.mkSeq 1308 1309
  , H.mkAssign 1309 "next_instr" (H.Num 0)
  , H.mkSeq 1309 1310
  , H.mkVar 1310 "__CLABEL_PREDICTED_CALL"
  , H.mkSeq 1310 1311
  , H.mkVar 1311 "NOP_1311"
  , H.mkVar 1312 "this_instr"
  , H.mkSeq 1312 1313
  , H.mkAssign 1313 "opcode" (H.Num 0)
  , H.mkSeq 1313 1314
  , H.mkVar 1314 "callable"
  , H.mkSeq 1314 1315
  , H.mkVar 1315 "self_or_null"
  , H.mkSeq 1315 1316
  , H.mkVar 1316 "args"
  , H.mkSeq 1316 1317
  , H.mkVar 1317 "res"
  , H.mkSeq 1317 1318
  , H.mkAssign 1318 "self_or_null" (H.Num 0)
  , H.mkSeq 1318 1319
  , H.mkAssign 1319 "callable" (H.Num 0)
  , H.mkSeq 1319 1320
  , H.mkVar 1320 "counter"
  , H.mkSeq 1320 1321
  , H.mkBranch 1321 (H.Eq (H.Num 0) (H.Num 1)) 1323 1326
  , H.mkAssign 1323 "next_instr" (H.Num 0)
  , H.mkSeq 1323 1324
  , H.mkAssign 1324 "stack_pointer" (H.Num 0)
  , H.mkSeq 1324 1325
  , H.mkAssign 1325 "opcode" (H.Num 0)
  , H.mkSeq 1325 1326
  , H.mkSeq 1325 35
  , H.mkSeq 1325 1327
  , H.mkVar 1326 "NOP_1326"
  , H.mkSeq 1326 1327
  , H.mkVar 1327 "IF_ELSE_FOOTER"
  , H.mkAssign 1328 "undefed" (H.Num 0)
  , H.mkSeq 1328 1329
  , H.mkBranch 1329 (H.Eq (H.Num 0) (H.Num 1)) 1330 1331
  , H.mkAssign 1330 "undefed" (H.Num 0)
  , H.mkSeq 1330 1329
  , H.mkVar 1331 "LOOP_FOOTER"
  , H.mkSeq 1331 1332
  , H.mkBranch 1332 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Plus (H.Id "PyMethod_Type") (H.Num 0))) (H.Plus (H.Num 0) (H.Num 0))) (H.Num 1)) 1334 1343
  , H.mkVar 1334 "callable_o"
  , H.mkSeq 1334 1335
  , H.mkVar 1335 "self"
  , H.mkSeq 1335 1336
  , H.mkAssign 1336 "self_or_null" (H.Num 0)
  , H.mkSeq 1336 1337
  , H.mkVar 1337 "method"
  , H.mkSeq 1337 1338
  , H.mkVar 1338 "temp"
  , H.mkSeq 1338 1339
  , H.mkAssign 1339 "callable" (H.Num 0)
  , H.mkSeq 1339 1340
  , H.mkAssign 1340 "undefed" (H.Num 0)
  , H.mkSeq 1340 1341
  , H.mkAssign 1341 "undefed" (H.Num 0)
  , H.mkSeq 1341 1342
  , H.mkAssign 1342 "stack_pointer" (H.Num 0)
  , H.mkSeq 1342 1343
  , H.mkSeq 1342 1344
  , H.mkVar 1343 "NOP_1343"
  , H.mkSeq 1343 1344
  , H.mkVar 1344 "IF_ELSE_FOOTER"
  , H.mkAssign 1345 "args" (H.Num 0)
  , H.mkSeq 1345 1346
  , H.mkVar 1346 "callable_o"
  , H.mkSeq 1346 1347
  , H.mkVar 1347 "total_args"
  , H.mkSeq 1347 1348
  , H.mkVar 1348 "arguments"
  , H.mkSeq 1348 1349
  , H.mkBranch 1349 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Num 0)) (H.Num 0)) (H.Num 1)) 1351 1352
  , H.mkAssign 1351 "total_args" (H.Num 0)
  , H.mkSeq 1351 1352
  , H.mkSeq 1351 1353
  , H.mkVar 1352 "NOP_1352"
  , H.mkSeq 1352 1353
  , H.mkVar 1353 "IF_ELSE_FOOTER"
  , H.mkBranch 1354 (H.Eq (H.Plus (H.Plus (H.Plus (H.Num 0) (H.Plus (H.Id "PyFunction_Type") (H.Num 0))) (H.Plus (H.Num 0) (H.Num 0))) (H.Plus (H.Num 0) (H.Id "_PyFunction_Vectorcall"))) (H.Num 1)) 1356 1372
  , H.mkVar 1356 "code_flags"
  , H.mkSeq 1356 1357
  , H.mkVar 1357 "locals"
  , H.mkSeq 1357 1358
  , H.mkAssign 1358 "undefed" (H.Num 0)
  , H.mkSeq 1358 1359
  , H.mkAssign 1359 "undefed" (H.Num 0)
  , H.mkSeq 1359 1360
  , H.mkVar 1360 "new_frame"
  , H.mkSeq 1360 1361
  , H.mkAssign 1361 "stack_pointer" (H.Num 0)
  , H.mkSeq 1361 1362
  , H.mkAssign 1362 "stack_pointer" (H.Num 0)
  , H.mkSeq 1362 1363
  , H.mkBranch 1363 (H.Eq (H.Plus (H.Id "new_frame") (H.Num 0)) (H.Num 1)) 1365 1365
  , H.mkSeq 1364 1817
  , H.mkSeq 1364 1366
  , H.mkVar 1365 "NOP_1365"
  , H.mkSeq 1365 1366
  , H.mkVar 1366 "IF_ELSE_FOOTER"
  , H.mkAssign 1367 "undefed" (H.Num 0)
  , H.mkSeq 1367 1368
  , H.mkAssign 1368 "frame" (H.Num 0)
  , H.mkSeq 1368 1369
  , H.mkSeq 1368 1886
  , H.mkBranch 1369 (H.Eq (H.Num 0) (H.Num 1)) 1370 1371
  , H.mkAssign 1370 "frame" (H.Num 0)
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
  , H.mkBranch 1376 (H.Eq (H.Plus (H.Id "args_o") (H.Num 0)) (H.Num 1)) 1378 1394
  , H.mkVar 1378 "tmp"
  , H.mkSeq 1378 1379
  , H.mkVar 1379 "_i"
  , H.mkSeq 1379 1380
  , H.mkBranch 1380 (H.Eq (H.Plus (H.Plus (H.Id "_i") (H.Num 0)) (H.Num 0)) (H.Num 1)) 1381 1385
  , H.mkAssign 1381 "tmp" (H.Num 0)
  , H.mkSeq 1381 1382
  , H.mkAssign 1382 "undefed" (H.Num 0)
  , H.mkSeq 1382 1383
  , H.mkAssign 1383 "undefed" (H.Num 0)
  , H.mkSeq 1383 1384
  , H.mkAssign 1384 "undefed" (H.Num 0)
  , H.mkSeq 1384 1385
  , H.mkSeq 1384 1380
  , H.mkVar 1385 "LOOP_FOOTER"
  , H.mkSeq 1385 1386
  , H.mkAssign 1386 "tmp" (H.Num 0)
  , H.mkSeq 1386 1387
  , H.mkAssign 1387 "self_or_null" (H.Num 0)
  , H.mkSeq 1387 1388
  , H.mkAssign 1388 "undefed" (H.Num 0)
  , H.mkSeq 1388 1389
  , H.mkAssign 1389 "tmp" (H.Num 0)
  , H.mkSeq 1389 1390
  , H.mkAssign 1390 "callable" (H.Num 0)
  , H.mkSeq 1390 1391
  , H.mkAssign 1391 "undefed" (H.Num 0)
  , H.mkSeq 1391 1392
  , H.mkAssign 1392 "stack_pointer" (H.Num 0)
  , H.mkSeq 1392 1393
  , H.mkAssign 1393 "stack_pointer" (H.Num 0)
  , H.mkSeq 1393 1394
  , H.mkSeq 1393 1817
  , H.mkSeq 1393 1395
  , H.mkVar 1394 "NOP_1394"
  , H.mkSeq 1394 1395
  , H.mkVar 1395 "IF_ELSE_FOOTER"
  , H.mkAssign 1396 "undefed" (H.Num 0)
  , H.mkSeq 1396 1397
  , H.mkAssign 1397 "undefed" (H.Num 0)
  , H.mkSeq 1397 1398
  , H.mkVar 1398 "res_o"
  , H.mkSeq 1398 1399
  , H.mkAssign 1399 "stack_pointer" (H.Num 0)
  , H.mkSeq 1399 1400
  , H.mkBranch 1400 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 1402 1514
  , H.mkVar 1402 "arg"
  , H.mkSeq 1402 1403
  , H.mkBranch 1403 (H.Eq (H.Plus (H.Id "res_o") (H.Num 0)) (H.Num 1)) 1405 1406
  , H.mkAssign 1405 "stack_pointer" (H.Num 0)
  , H.mkSeq 1405 1513
  , H.mkVar 1406 "err"
  , H.mkSeq 1406 1407
  , H.mkAssign 1407 "stack_pointer" (H.Num 0)
  , H.mkSeq 1407 1408
  , H.mkBranch 1408 (H.Eq (H.Plus (H.Id "err") (H.Num 0)) (H.Num 1)) 1410 1511
  , H.mkAssign 1410 "undefed" (H.Num 0)
  , H.mkSeq 1410 1411
  , H.mkAssign 1411 "_tmp_old_op" (H.Num 0)
  , H.mkSeq 1411 1412
  , H.mkBranch 1412 (H.Eq (H.Plus (H.Id "_tmp_old_op") (H.Num 0)) (H.Num 1)) 1414 1457
  , H.mkAssign 1414 "undefed" (H.Num 0)
  , H.mkSeq 1414 1415
  , H.mkVar 1415 "op"
  , H.mkSeq 1415 1416
  , H.mkBranch 1416 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Num 0)) (H.Num 0)) (H.Num 1)) 1418 1433
  , H.mkVar 1418 "tracer"
  , H.mkSeq 1418 1419
  , H.mkBranch 1419 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 1421 1422
  , H.mkVar 1421 "data"
  , H.mkSeq 1421 1422
  , H.mkSeq 1421 1423
  , H.mkVar 1422 "NOP_1422"
  , H.mkSeq 1422 1423
  , H.mkVar 1423 "IF_ELSE_FOOTER"
  , H.mkBranch 1424 (H.Eq (H.Num 0) (H.Num 1)) 1425 1431
  , H.mkVar 1425 "tracer"
  , H.mkSeq 1425 1426
  , H.mkBranch 1426 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 1428 1429
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
  , H.mkBranch 1435 (H.Eq (H.Num 0) (H.Num 1)) 1436 1456
  , H.mkVar 1436 "op"
  , H.mkSeq 1436 1437
  , H.mkBranch 1437 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Num 0)) (H.Num 0)) (H.Num 1)) 1439 1454
  , H.mkVar 1439 "tracer"
  , H.mkSeq 1439 1440
  , H.mkBranch 1440 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 1442 1443
  , H.mkVar 1442 "data"
  , H.mkSeq 1442 1443
  , H.mkSeq 1442 1444
  , H.mkVar 1443 "NOP_1443"
  , H.mkSeq 1443 1444
  , H.mkVar 1444 "IF_ELSE_FOOTER"
  , H.mkBranch 1445 (H.Eq (H.Num 0) (H.Num 1)) 1446 1452
  , H.mkVar 1446 "tracer"
  , H.mkSeq 1446 1447
  , H.mkBranch 1447 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 1449 1450
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
  , H.mkBranch 1459 (H.Eq (H.Num 0) (H.Num 1)) 1460 1509
  , H.mkAssign 1460 "undefed" (H.Num 0)
  , H.mkSeq 1460 1461
  , H.mkAssign 1461 "_tmp_old_op" (H.Num 0)
  , H.mkSeq 1461 1462
  , H.mkBranch 1462 (H.Eq (H.Plus (H.Id "_tmp_old_op") (H.Num 0)) (H.Num 1)) 1464 1507
  , H.mkAssign 1464 "undefed" (H.Num 0)
  , H.mkSeq 1464 1465
  , H.mkVar 1465 "op"
  , H.mkSeq 1465 1466
  , H.mkBranch 1466 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Num 0)) (H.Num 0)) (H.Num 1)) 1468 1483
  , H.mkVar 1468 "tracer"
  , H.mkSeq 1468 1469
  , H.mkBranch 1469 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 1471 1472
  , H.mkVar 1471 "data"
  , H.mkSeq 1471 1472
  , H.mkSeq 1471 1473
  , H.mkVar 1472 "NOP_1472"
  , H.mkSeq 1472 1473
  , H.mkVar 1473 "IF_ELSE_FOOTER"
  , H.mkBranch 1474 (H.Eq (H.Num 0) (H.Num 1)) 1475 1481
  , H.mkVar 1475 "tracer"
  , H.mkSeq 1475 1476
  , H.mkBranch 1476 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 1478 1479
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
  , H.mkBranch 1485 (H.Eq (H.Num 0) (H.Num 1)) 1486 1506
  , H.mkVar 1486 "op"
  , H.mkSeq 1486 1487
  , H.mkBranch 1487 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Num 0)) (H.Num 0)) (H.Num 1)) 1489 1504
  , H.mkVar 1489 "tracer"
  , H.mkSeq 1489 1490
  , H.mkBranch 1490 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 1492 1493
  , H.mkVar 1492 "data"
  , H.mkSeq 1492 1493
  , H.mkSeq 1492 1494
  , H.mkVar 1493 "NOP_1493"
  , H.mkSeq 1493 1494
  , H.mkVar 1494 "IF_ELSE_FOOTER"
  , H.mkBranch 1495 (H.Eq (H.Num 0) (H.Num 1)) 1496 1502
  , H.mkVar 1496 "tracer"
  , H.mkSeq 1496 1497
  , H.mkBranch 1497 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 1499 1500
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
  , H.mkAssign 1510 "stack_pointer" (H.Num 0)
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
  , H.mkBranch 1518 (H.Eq (H.Plus (H.Plus (H.Id "_i") (H.Num 0)) (H.Num 0)) (H.Num 1)) 1519 1521
  , H.mkAssign 1519 "tmp" (H.Num 0)
  , H.mkSeq 1519 1520
  , H.mkAssign 1520 "undefed" (H.Num 0)
  , H.mkSeq 1520 1521
  , H.mkSeq 1520 1518
  , H.mkVar 1521 "LOOP_FOOTER"
  , H.mkSeq 1521 1522
  , H.mkAssign 1522 "tmp" (H.Num 0)
  , H.mkSeq 1522 1523
  , H.mkAssign 1523 "self_or_null" (H.Num 0)
  , H.mkSeq 1523 1524
  , H.mkAssign 1524 "undefed" (H.Num 0)
  , H.mkSeq 1524 1525
  , H.mkAssign 1525 "tmp" (H.Num 0)
  , H.mkSeq 1525 1526
  , H.mkAssign 1526 "callable" (H.Num 0)
  , H.mkSeq 1526 1527
  , H.mkAssign 1527 "undefed" (H.Num 0)
  , H.mkSeq 1527 1528
  , H.mkAssign 1528 "stack_pointer" (H.Num 0)
  , H.mkSeq 1528 1529
  , H.mkAssign 1529 "stack_pointer" (H.Num 0)
  , H.mkSeq 1529 1530
  , H.mkBranch 1530 (H.Eq (H.Plus (H.Id "res_o") (H.Num 0)) (H.Num 1)) 1532 1532
  , H.mkSeq 1531 1817
  , H.mkSeq 1531 1533
  , H.mkVar 1532 "NOP_1532"
  , H.mkSeq 1532 1533
  , H.mkVar 1533 "IF_ELSE_FOOTER"
  , H.mkAssign 1534 "res" (H.Num 0)
  , H.mkSeq 1534 1535
  , H.mkAssign 1535 "undefed" (H.Num 0)
  , H.mkSeq 1535 1536
  , H.mkAssign 1536 "stack_pointer" (H.Num 0)
  , H.mkSeq 1536 1537
  , H.mkVar 1537 "err"
  , H.mkSeq 1537 1538
  , H.mkAssign 1538 "stack_pointer" (H.Num 0)
  , H.mkSeq 1538 1539
  , H.mkBranch 1539 (H.Eq (H.Plus (H.Id "err") (H.Num 0)) (H.Num 1)) 1541 1541
  , H.mkSeq 1540 1817
  , H.mkSeq 1540 1542
  , H.mkVar 1541 "NOP_1541"
  , H.mkSeq 1541 1542
  , H.mkVar 1542 "IF_ELSE_FOOTER"
  , H.mkVar 1543 "word"
  , H.mkSeq 1543 1544
  , H.mkAssign 1544 "opcode" (H.Num 0)
  , H.mkSeq 1544 1545
  , H.mkAssign 1545 "oparg" (H.Num 0)
  , H.mkSeq 1545 1546
  , H.mkBranch 1546 (H.Eq (H.Num 0) (H.Num 1)) 1547 1550
  , H.mkVar 1547 "word"
  , H.mkSeq 1547 1548
  , H.mkAssign 1548 "opcode" (H.Num 0)
  , H.mkSeq 1548 1549
  , H.mkAssign 1549 "oparg" (H.Num 0)
  , H.mkSeq 1549 1550
  , H.mkSeq 1549 1546
  , H.mkVar 1550 "LOOP_FOOTER"
  , H.mkSeq 1550 1551
  , H.mkSeq 1550 35
  , H.mkBranch 1551 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 1553 1637
  , H.mkVar 1553 "NOP_1553"
  , H.mkVar 1554 "__CLABEL_TARGET_CALL_ALLOC_AND_ENTER_INIT"
  , H.mkSeq 1554 1555
  , H.mkVar 1555 "this_instr"
  , H.mkSeq 1555 1556
  , H.mkAssign 1556 "undefed" (H.Num 0)
  , H.mkSeq 1556 1557
  , H.mkAssign 1557 "next_instr" (H.Num 0)
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
  , H.mkBranch 1565 (H.Eq (H.Num 0) (H.Num 1)) 1567 1567
  , H.mkSeq 1566 1310
  , H.mkSeq 1566 1568
  , H.mkVar 1567 "NOP_1567"
  , H.mkSeq 1567 1568
  , H.mkVar 1568 "IF_ELSE_FOOTER"
  , H.mkAssign 1569 "self_or_null" (H.Num 0)
  , H.mkSeq 1569 1570
  , H.mkAssign 1570 "callable" (H.Num 0)
  , H.mkSeq 1570 1571
  , H.mkVar 1571 "type_version"
  , H.mkSeq 1571 1572
  , H.mkVar 1572 "callable_o"
  , H.mkSeq 1572 1573
  , H.mkBranch 1573 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Num 0)) (H.Num 0)) (H.Num 1)) 1575 1575
  , H.mkSeq 1574 1310
  , H.mkSeq 1574 1576
  , H.mkVar 1575 "NOP_1575"
  , H.mkSeq 1575 1576
  , H.mkVar 1576 "IF_ELSE_FOOTER"
  , H.mkBranch 1577 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 1579 1579
  , H.mkSeq 1578 1310
  , H.mkSeq 1578 1580
  , H.mkVar 1579 "NOP_1579"
  , H.mkSeq 1579 1580
  , H.mkVar 1580 "IF_ELSE_FOOTER"
  , H.mkVar 1581 "tp"
  , H.mkSeq 1581 1582
  , H.mkBranch 1582 (H.Eq (H.Plus (H.Num 0) (H.Id "type_version")) (H.Num 1)) 1584 1584
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
  , H.mkBranch 1589 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 1591 1591
  , H.mkSeq 1590 1310
  , H.mkSeq 1590 1592
  , H.mkVar 1591 "NOP_1591"
  , H.mkSeq 1591 1592
  , H.mkVar 1592 "IF_ELSE_FOOTER"
  , H.mkVar 1593 "self_o"
  , H.mkSeq 1593 1594
  , H.mkAssign 1594 "stack_pointer" (H.Num 0)
  , H.mkSeq 1594 1595
  , H.mkBranch 1595 (H.Eq (H.Plus (H.Id "self_o") (H.Num 0)) (H.Num 1)) 1597 1597
  , H.mkSeq 1596 1817
  , H.mkSeq 1596 1598
  , H.mkVar 1597 "NOP_1597"
  , H.mkSeq 1597 1598
  , H.mkVar 1598 "IF_ELSE_FOOTER"
  , H.mkAssign 1599 "self_or_null" (H.Num 0)
  , H.mkSeq 1599 1600
  , H.mkVar 1600 "temp"
  , H.mkSeq 1600 1601
  , H.mkAssign 1601 "callable" (H.Num 0)
  , H.mkSeq 1601 1602
  , H.mkAssign 1602 "undefed" (H.Num 0)
  , H.mkSeq 1602 1603
  , H.mkAssign 1603 "undefed" (H.Num 0)
  , H.mkSeq 1603 1604
  , H.mkAssign 1604 "stack_pointer" (H.Num 0)
  , H.mkSeq 1604 1605
  , H.mkAssign 1605 "args" (H.Num 0)
  , H.mkSeq 1605 1606
  , H.mkAssign 1606 "self" (H.Num 0)
  , H.mkSeq 1606 1607
  , H.mkAssign 1607 "init" (H.Num 0)
  , H.mkSeq 1607 1608
  , H.mkVar 1608 "shim"
  , H.mkSeq 1608 1609
  , H.mkAssign 1609 "stack_pointer" (H.Num 0)
  , H.mkSeq 1609 1610
  , H.mkAssign 1610 "undefed" (H.Num 0)
  , H.mkSeq 1610 1611
  , H.mkVar 1611 "temp"
  , H.mkSeq 1611 1612
  , H.mkAssign 1612 "stack_pointer" (H.Num 0)
  , H.mkSeq 1612 1613
  , H.mkAssign 1613 "stack_pointer" (H.Num 0)
  , H.mkSeq 1613 1614
  , H.mkBranch 1614 (H.Eq (H.Plus (H.Id "temp") (H.Num 0)) (H.Num 1)) 1616 1617
  , H.mkAssign 1616 "stack_pointer" (H.Num 0)
  , H.mkSeq 1616 1617
  , H.mkSeq 1616 1817
  , H.mkSeq 1616 1618
  , H.mkVar 1617 "NOP_1617"
  , H.mkSeq 1617 1618
  , H.mkVar 1618 "IF_ELSE_FOOTER"
  , H.mkAssign 1619 "undefed" (H.Num 0)
  , H.mkSeq 1619 1620
  , H.mkAssign 1620 "init_frame" (H.Num 0)
  , H.mkSeq 1620 1621
  , H.mkAssign 1621 "new_frame" (H.Num 0)
  , H.mkSeq 1621 1622
  , H.mkVar 1622 "temp"
  , H.mkSeq 1622 1623
  , H.mkAssign 1623 "frame" (H.Num 0)
  , H.mkSeq 1623 1624
  , H.mkAssign 1624 "stack_pointer" (H.Num 0)
  , H.mkSeq 1624 1625
  , H.mkAssign 1625 "next_instr" (H.Num 0)
  , H.mkSeq 1625 1626
  , H.mkBranch 1626 (H.Eq (H.Num 0) (H.Num 1)) 1627 1628
  , H.mkAssign 1627 "next_instr" (H.Num 0)
  , H.mkSeq 1627 1626
  , H.mkVar 1628 "LOOP_FOOTER"
  , H.mkSeq 1628 1629
  , H.mkVar 1629 "word"
  , H.mkSeq 1629 1630
  , H.mkAssign 1630 "opcode" (H.Num 0)
  , H.mkSeq 1630 1631
  , H.mkAssign 1631 "oparg" (H.Num 0)
  , H.mkSeq 1631 1632
  , H.mkBranch 1632 (H.Eq (H.Num 0) (H.Num 1)) 1633 1636
  , H.mkVar 1633 "word"
  , H.mkSeq 1633 1634
  , H.mkAssign 1634 "opcode" (H.Num 0)
  , H.mkSeq 1634 1635
  , H.mkAssign 1635 "oparg" (H.Num 0)
  , H.mkSeq 1635 1636
  , H.mkSeq 1635 1632
  , H.mkVar 1636 "LOOP_FOOTER"
  , H.mkSeq 1636 1637
  , H.mkSeq 1636 35
  , H.mkBranch 1637 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 1639 1727
  , H.mkVar 1639 "NOP_1639"
  , H.mkVar 1640 "__CLABEL_TARGET_CALL_BOUND_METHOD_EXACT_ARGS"
  , H.mkSeq 1640 1641
  , H.mkVar 1641 "this_instr"
  , H.mkSeq 1641 1642
  , H.mkAssign 1642 "undefed" (H.Num 0)
  , H.mkSeq 1642 1643
  , H.mkAssign 1643 "next_instr" (H.Num 0)
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
  , H.mkBranch 1649 (H.Eq (H.Num 0) (H.Num 1)) 1651 1651
  , H.mkSeq 1650 1310
  , H.mkSeq 1650 1652
  , H.mkVar 1651 "NOP_1651"
  , H.mkSeq 1651 1652
  , H.mkVar 1652 "IF_ELSE_FOOTER"
  , H.mkAssign 1653 "null" (H.Num 0)
  , H.mkSeq 1653 1654
  , H.mkAssign 1654 "callable" (H.Num 0)
  , H.mkSeq 1654 1655
  , H.mkBranch 1655 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Num 0)) (H.Num 0)) (H.Num 1)) 1657 1657
  , H.mkSeq 1656 1310
  , H.mkSeq 1656 1658
  , H.mkVar 1657 "NOP_1657"
  , H.mkSeq 1657 1658
  , H.mkVar 1658 "IF_ELSE_FOOTER"
  , H.mkBranch 1659 (H.Eq (H.Plus (H.Num 0) (H.Plus (H.Id "PyMethod_Type") (H.Num 0))) (H.Num 1)) 1661 1661
  , H.mkSeq 1660 1310
  , H.mkSeq 1660 1662
  , H.mkVar 1661 "NOP_1661"
  , H.mkSeq 1661 1662
  , H.mkVar 1662 "IF_ELSE_FOOTER"
  , H.mkAssign 1663 "self_or_null" (H.Num 0)
  , H.mkSeq 1663 1664
  , H.mkVar 1664 "callable_o"
  , H.mkSeq 1664 1665
  , H.mkAssign 1665 "self_or_null" (H.Num 0)
  , H.mkSeq 1665 1666
  , H.mkVar 1666 "temp"
  , H.mkSeq 1666 1667
  , H.mkAssign 1667 "callable" (H.Num 0)
  , H.mkSeq 1667 1668
  , H.mkAssign 1668 "undefed" (H.Num 0)
  , H.mkSeq 1668 1669
  , H.mkAssign 1669 "undefed" (H.Num 0)
  , H.mkSeq 1669 1670
  , H.mkAssign 1670 "stack_pointer" (H.Num 0)
  , H.mkSeq 1670 1671
  , H.mkVar 1671 "func_version"
  , H.mkSeq 1671 1672
  , H.mkVar 1672 "callable_o"
  , H.mkSeq 1672 1673
  , H.mkBranch 1673 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Plus (H.Id "PyFunction_Type") (H.Num 0))) (H.Num 0)) (H.Num 1)) 1675 1675
  , H.mkSeq 1674 1310
  , H.mkSeq 1674 1676
  , H.mkVar 1675 "NOP_1675"
  , H.mkSeq 1675 1676
  , H.mkVar 1676 "IF_ELSE_FOOTER"
  , H.mkVar 1677 "func"
  , H.mkSeq 1677 1678
  , H.mkBranch 1678 (H.Eq (H.Plus (H.Num 0) (H.Id "func_version")) (H.Num 1)) 1680 1680
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
  , H.mkBranch 1685 (H.Eq (H.Plus (H.Num 0) (H.Plus (H.Id "oparg") (H.Plus (H.Plus (H.Num 0) (H.Num 0)) (H.Num 0)))) (H.Num 1)) 1687 1687
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
  , H.mkBranch 1692 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 1694 1694
  , H.mkSeq 1693 1310
  , H.mkSeq 1693 1695
  , H.mkVar 1694 "NOP_1694"
  , H.mkSeq 1694 1695
  , H.mkVar 1695 "IF_ELSE_FOOTER"
  , H.mkBranch 1696 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 1698 1698
  , H.mkSeq 1697 1310
  , H.mkSeq 1697 1699
  , H.mkVar 1698 "NOP_1698"
  , H.mkSeq 1698 1699
  , H.mkVar 1699 "IF_ELSE_FOOTER"
  , H.mkAssign 1700 "args" (H.Num 0)
  , H.mkSeq 1700 1701
  , H.mkVar 1701 "has_self"
  , H.mkSeq 1701 1702
  , H.mkVar 1702 "pushed_frame"
  , H.mkSeq 1702 1703
  , H.mkVar 1703 "first_non_self_local"
  , H.mkSeq 1703 1704
  , H.mkAssign 1704 "undefed" (H.Num 0)
  , H.mkSeq 1704 1705
  , H.mkVar 1705 "i"
  , H.mkSeq 1705 1706
  , H.mkBranch 1706 (H.Eq (H.Plus (H.Id "i") (H.Id "oparg")) (H.Num 1)) 1707 1708
  , H.mkAssign 1707 "undefed" (H.Num 0)
  , H.mkSeq 1707 1708
  , H.mkSeq 1707 1706
  , H.mkVar 1708 "LOOP_FOOTER"
  , H.mkSeq 1708 1709
  , H.mkAssign 1709 "new_frame" (H.Num 0)
  , H.mkSeq 1709 1710
  , H.mkAssign 1710 "undefed" (H.Num 0)
  , H.mkSeq 1710 1711
  , H.mkVar 1711 "temp"
  , H.mkSeq 1711 1712
  , H.mkAssign 1712 "stack_pointer" (H.Num 0)
  , H.mkSeq 1712 1713
  , H.mkAssign 1713 "frame" (H.Num 0)
  , H.mkSeq 1713 1714
  , H.mkAssign 1714 "stack_pointer" (H.Num 0)
  , H.mkSeq 1714 1715
  , H.mkAssign 1715 "next_instr" (H.Num 0)
  , H.mkSeq 1715 1716
  , H.mkBranch 1716 (H.Eq (H.Num 0) (H.Num 1)) 1717 1718
  , H.mkAssign 1717 "next_instr" (H.Num 0)
  , H.mkSeq 1717 1716
  , H.mkVar 1718 "LOOP_FOOTER"
  , H.mkSeq 1718 1719
  , H.mkVar 1719 "word"
  , H.mkSeq 1719 1720
  , H.mkAssign 1720 "opcode" (H.Num 0)
  , H.mkSeq 1720 1721
  , H.mkAssign 1721 "oparg" (H.Num 0)
  , H.mkSeq 1721 1722
  , H.mkBranch 1722 (H.Eq (H.Num 0) (H.Num 1)) 1723 1726
  , H.mkVar 1723 "word"
  , H.mkSeq 1723 1724
  , H.mkAssign 1724 "opcode" (H.Num 0)
  , H.mkSeq 1724 1725
  , H.mkAssign 1725 "oparg" (H.Num 0)
  , H.mkSeq 1725 1726
  , H.mkSeq 1725 1722
  , H.mkVar 1726 "LOOP_FOOTER"
  , H.mkSeq 1726 1727
  , H.mkSeq 1726 35
  , H.mkBranch 1727 (H.Eq (H.Plus (H.Id "opcode") (H.Num 0)) (H.Num 1)) 1729 1810
  , H.mkVar 1729 "NOP_1729"
  , H.mkVar 1730 "__CLABEL_TARGET_CALL_BOUND_METHOD_GENERAL"
  , H.mkSeq 1730 1731
  , H.mkVar 1731 "this_instr"
  , H.mkSeq 1731 1732
  , H.mkAssign 1732 "undefed" (H.Num 0)
  , H.mkSeq 1732 1733
  , H.mkAssign 1733 "next_instr" (H.Num 0)
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
  , H.mkBranch 1739 (H.Eq (H.Num 0) (H.Num 1)) 1741 1741
  , H.mkSeq 1740 1310
  , H.mkSeq 1740 1742
  , H.mkVar 1741 "NOP_1741"
  , H.mkSeq 1741 1742
  , H.mkVar 1742 "IF_ELSE_FOOTER"
  , H.mkAssign 1743 "null" (H.Num 0)
  , H.mkSeq 1743 1744
  , H.mkAssign 1744 "callable" (H.Num 0)
  , H.mkSeq 1744 1745
  , H.mkVar 1745 "func_version"
  , H.mkSeq 1745 1746
  , H.mkVar 1746 "callable_o"
  , H.mkSeq 1746 1747
  , H.mkBranch 1747 (H.Eq (H.Plus (H.Num 0) (H.Plus (H.Id "PyMethod_Type") (H.Num 0))) (H.Num 1)) 1749 1749
  , H.mkSeq 1748 1310
  , H.mkSeq 1748 1750
  , H.mkVar 1749 "NOP_1749"
  , H.mkSeq 1749 1750
  , H.mkVar 1750 "IF_ELSE_FOOTER"
  , H.mkVar 1751 "func"
  , H.mkSeq 1751 1752
  , H.mkBranch 1752 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Plus (H.Id "PyFunction_Type") (H.Num 0))) (H.Num 0)) (H.Num 1)) 1754 1754
  , H.mkSeq 1753 1310
  , H.mkSeq 1753 1755
  , H.mkVar 1754 "NOP_1754"
  , H.mkSeq 1754 1755
  , H.mkVar 1755 "IF_ELSE_FOOTER"
  , H.mkBranch 1756 (H.Eq (H.Plus (H.Num 0) (H.Id "func_version")) (H.Num 1)) 1758 1758
  , H.mkSeq 1757 1310
  , H.mkSeq 1757 1759
  , H.mkVar 1758 "NOP_1758"
  , H.mkSeq 1758 1759
  , H.mkVar 1759 "IF_ELSE_FOOTER"
  , H.mkBranch 1760 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Num 0)) (H.Num 0)) (H.Num 1)) 1762 1762
  , H.mkSeq 1761 1310
  , H.mkSeq 1761 1763
  , H.mkVar 1762 "NOP_1762"
  , H.mkSeq 1762 1763
  , H.mkVar 1763 "IF_ELSE_FOOTER"
  , H.mkAssign 1764 "self_or_null" (H.Num 0)
  , H.mkSeq 1764 1765
  , H.mkVar 1765 "callable_o"
  , H.mkSeq 1765 1766
  , H.mkAssign 1766 "self_or_null" (H.Num 0)
  , H.mkSeq 1766 1767
  , H.mkVar 1767 "temp"
  , H.mkSeq 1767 1768
  , H.mkAssign 1768 "callable" (H.Num 0)
  , H.mkSeq 1768 1769
  , H.mkAssign 1769 "undefed" (H.Num 0)
  , H.mkSeq 1769 1770
  , H.mkAssign 1770 "undefed" (H.Num 0)
  , H.mkSeq 1770 1771
  , H.mkAssign 1771 "stack_pointer" (H.Num 0)
  , H.mkSeq 1771 1772
  , H.mkBranch 1772 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 1774 1774
  , H.mkSeq 1773 1310
  , H.mkSeq 1773 1775
  , H.mkVar 1774 "NOP_1774"
  , H.mkSeq 1774 1775
  , H.mkVar 1775 "IF_ELSE_FOOTER"
  , H.mkAssign 1776 "args" (H.Num 0)
  , H.mkSeq 1776 1777
  , H.mkVar 1777 "callable_o"
  , H.mkSeq 1777 1778
  , H.mkVar 1778 "total_args"
  , H.mkSeq 1778 1779
  , H.mkBranch 1779 (H.Eq (H.Plus (H.Plus (H.Num 0) (H.Num 0)) (H.Num 0)) (H.Num 1)) 1781 1782
  , H.mkAssign 1781 "total_args" (H.Num 0)
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
  , H.mkAssign 1787 "stack_pointer" (H.Num 0)
  , H.mkSeq 1787 1788
  , H.mkAssign 1788 "stack_pointer" (H.Num 0)
  , H.mkSeq 1788 1789
  , H.mkBranch 1789 (H.Eq (H.Plus (H.Id "temp") (H.Num 0)) (H.Num 1)) 1791 1791
  , H.mkSeq 1790 1817
  , H.mkSeq 1790 1792
  , H.mkVar 1791 "NOP_1791"
  , H.mkSeq 1791 1792
  , H.mkVar 1792 "IF_ELSE_FOOTER"
  , H.mkAssign 1793 "new_frame" (H.Num 0)
  , H.mkSeq 1793 1794
  , H.mkAssign 1794 "undefed" (H.Num 0)
  , H.mkSeq 1794 1795
  , H.mkVar 1795 "temp"
  , H.mkSeq 1795 1796
  , H.mkAssign 1796 "frame" (H.Num 0)
  , H.mkSeq 1796 1797
  , H.mkAssign 1797 "stack_pointer" (H.Num 0)
  , H.mkSeq 1797 1798
  , H.mkAssign 1798 "next_instr" (H.Num 0)
  , H.mkSeq 1798 1799
  , H.mkBranch 1799 (H.Eq (H.Num 0) (H.Num 1)) 1800 1801
  , H.mkAssign 1800 "next_instr" (H.Num 0)
  , H.mkSeq 1800 1799
  , H.mkVar 1801 "LOOP_FOOTER"
  , H.mkSeq 1801 1802
  , H.mkVar 1802 "word"
  , H.mkSeq 1802 1803
  , H.mkAssign 1803 "opcode" (H.Num 0)
  , H.mkSeq 1803 1804
  , H.mkAssign 1804 "oparg" (H.Num 0)
  , H.mkSeq 1804 1805
  , H.mkBranch 1805 (H.Eq (H.Num 0) (H.Num 1)) 1806 1809
  , H.mkVar 1806 "word"
  , H.mkSeq 1806 1807
  , H.mkAssign 1807 "opcode" (H.Num 0)
  , H.mkSeq 1807 1808
  , H.mkAssign 1808 "oparg" (H.Num 0)
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
  , H.mkAssign 1814 "stack_pointer" (H.Num 0)
  , H.mkSeq 1814 1815
  , H.mkSeq 1814 1817
  , H.mkVar 1815 "__CLABEL_pop_1_error"
  , H.mkSeq 1815 1816
  , H.mkAssign 1816 "stack_pointer" (H.Num 0)
  , H.mkSeq 1816 1817
  , H.mkSeq 1816 1817
  , H.mkVar 1817 "__CLABEL_error"
  , H.mkSeq 1817 1818
  , H.mkBranch 1818 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 1820 1821
  , H.mkAssign 1820 "stack_pointer" (H.Num 0)
  , H.mkSeq 1820 1822
  , H.mkVar 1821 "NOP_1821"
  , H.mkSeq 1821 1822
  , H.mkVar 1822 "IF_ELSE_FOOTER"
  , H.mkBranch 1823 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 1825 1832
  , H.mkVar 1825 "f"
  , H.mkSeq 1825 1826
  , H.mkAssign 1826 "stack_pointer" (H.Num 0)
  , H.mkSeq 1826 1827
  , H.mkBranch 1827 (H.Eq (H.Plus (H.Id "f") (H.Num 0)) (H.Num 1)) 1829 1830
  , H.mkAssign 1829 "stack_pointer" (H.Num 0)
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
  , H.mkBranch 1840 (H.Eq (H.Plus (H.Id "handled") (H.Num 0)) (H.Num 1)) 1842 1846
  , H.mkVar 1842 "stackbase"
  , H.mkSeq 1842 1843
  , H.mkBranch 1843 (H.Eq (H.Plus (H.Num 0) (H.Id "stackbase")) (H.Num 1)) 1844 1845
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
  , H.mkBranch 1849 (H.Eq (H.Plus (H.Num 0) (H.Id "new_top")) (H.Num 1)) 1850 1851
  , H.mkVar 1850 "ref"
  , H.mkSeq 1850 1851
  , H.mkSeq 1850 1849
  , H.mkVar 1851 "LOOP_FOOTER"
  , H.mkSeq 1851 1852
  , H.mkBranch 1852 (H.Eq (H.Id "lasti") (H.Num 1)) 1854 1856
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
  , H.mkAssign 1859 "next_instr" (H.Num 0)
  , H.mkSeq 1859 1860
  , H.mkVar 1860 "err"
  , H.mkSeq 1860 1861
  , H.mkBranch 1861 (H.Eq (H.Plus (H.Id "err") (H.Num 0)) (H.Num 1)) 1863 1863
  , H.mkSeq 1862 1834
  , H.mkSeq 1862 1864
  , H.mkVar 1863 "NOP_1863"
  , H.mkSeq 1863 1864
  , H.mkVar 1864 "IF_ELSE_FOOTER"
  , H.mkAssign 1865 "stack_pointer" (H.Num 0)
  , H.mkSeq 1865 1866
  , H.mkVar 1866 "word"
  , H.mkSeq 1866 1867
  , H.mkAssign 1867 "opcode" (H.Num 0)
  , H.mkSeq 1867 1868
  , H.mkAssign 1868 "oparg" (H.Num 0)
  , H.mkSeq 1868 1869
  , H.mkBranch 1869 (H.Eq (H.Num 0) (H.Num 1)) 1870 1873
  , H.mkVar 1870 "word"
  , H.mkSeq 1870 1871
  , H.mkAssign 1871 "opcode" (H.Num 0)
  , H.mkSeq 1871 1872
  , H.mkAssign 1872 "oparg" (H.Num 0)
  , H.mkSeq 1872 1873
  , H.mkSeq 1872 1869
  , H.mkVar 1873 "LOOP_FOOTER"
  , H.mkSeq 1873 1874
  , H.mkSeq 1873 35
  , H.mkVar 1874 "__CLABEL_exit_unwind"
  , H.mkSeq 1874 1875
  , H.mkVar 1875 "dying"
  , H.mkSeq 1875 1876
  , H.mkAssign 1876 "frame" (H.Num 0)
  , H.mkSeq 1876 1877
  , H.mkAssign 1877 "undefed" (H.Num 0)
  , H.mkSeq 1877 1878
  , H.mkBranch 1878 (H.Eq (H.Plus (H.Num 0) (H.Id "FRAME_OWNED_BY_INTERPRETER")) (H.Num 1)) 1880 1882
  , H.mkAssign 1880 "undefed" (H.Num 0)
  , H.mkSeq 1880 1881
  , H.mkAssign 1881 "return" (H.Num 0)
  , H.mkSeq 1881 1882
  , H.mkSeq 1881 1883
  , H.mkVar 1882 "NOP_1882"
  , H.mkSeq 1882 1883
  , H.mkVar 1883 "IF_ELSE_FOOTER"
  , H.mkAssign 1884 "next_instr" (H.Num 0)
  , H.mkSeq 1884 1885
  , H.mkAssign 1885 "stack_pointer" (H.Num 0)
  , H.mkSeq 1885 1886
  , H.mkSeq 1885 1817
  , H.mkVar 1886 "__CLABEL_start_frame"
  , H.mkSeq 1886 1887
  , H.mkVar 1887 "too_deep"
  , H.mkSeq 1887 1888
  , H.mkBranch 1888 (H.Eq (H.Id "too_deep") (H.Num 1)) 1890 1890
  , H.mkSeq 1889 1874
  , H.mkSeq 1889 1891
  , H.mkVar 1890 "NOP_1890"
  , H.mkSeq 1890 1891
  , H.mkVar 1891 "IF_ELSE_FOOTER"
  , H.mkAssign 1892 "next_instr" (H.Num 0)
  , H.mkSeq 1892 1893
  , H.mkAssign 1893 "stack_pointer" (H.Num 0)
  , H.mkSeq 1893 1894
  , H.mkVar 1894 "word"
  , H.mkSeq 1894 1895
  , H.mkAssign 1895 "opcode" (H.Num 0)
  , H.mkSeq 1895 1896
  , H.mkAssign 1896 "oparg" (H.Num 0)
  , H.mkSeq 1896 1897
  , H.mkBranch 1897 (H.Eq (H.Num 0) (H.Num 1)) 1898 1901
  , H.mkVar 1898 "word"
  , H.mkSeq 1898 1899
  , H.mkAssign 1899 "opcode" (H.Num 0)
  , H.mkSeq 1899 1900
  , H.mkAssign 1900 "oparg" (H.Num 0)
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
  , H.mkAssign 1905 "frame" (H.Num 0)
  , H.mkSeq 1905 1906
  , H.mkAssign 1906 "undefed" (H.Num 0)
  , H.mkSeq 1906 1907
  , H.mkAssign 1907 "undefed" (H.Num 0)
  , H.mkSeq 1907 1908
  , H.mkAssign 1908 "return" (H.Num 0)
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
  , NP.Branch 7 (NP.Eq (NP.Num 0) (NP.Num 1)) 9 10
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
  , NP.Branch 25 (NP.Eq (NP.Id "throwflag") (NP.Num 1)) 27 33
  , NP.Branch 27 (NP.Eq (NP.Num 0) (NP.Num 1)) 29 29
  , NP.Seq 28 1902
  , NP.Seq 28 30
  , NP.Var 29 "NOP_29"
  , NP.Seq 29 30
  , NP.Var 30 "IF_ELSE_FOOTER"
  , NP.Assign 31 "next_instr" (NP.Num 0)
  , NP.Seq 31 32
  , NP.Assign 32 "stack_pointer" (NP.Num 0)
  , NP.Seq 32 33
  , NP.Seq 32 1817
  , NP.Seq 32 34
  , NP.Var 33 "NOP_33"
  , NP.Seq 33 34
  , NP.Var 34 "IF_ELSE_FOOTER"
  , NP.Seq 34 1886
  , NP.Var 35 "__CLABEL_dispatch_opcode"
  , NP.Seq 35 36
  , NP.Branch 36 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 38 87
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
  , NP.Branch 51 (NP.Eq (NP.Num 0) (NP.Num 1)) 53 56
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
  , NP.Branch 59 (NP.Eq (NP.Num 0) (NP.Num 1)) 60 61
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
  , NP.Branch 66 (NP.Eq (NP.Plus (NP.Id "res_o") (NP.Num 0)) (NP.Num 1)) 68 68
  , NP.Seq 67 1817
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
  , NP.Branch 82 (NP.Eq (NP.Num 0) (NP.Num 1)) 83 86
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
  , NP.Branch 87 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 89 129
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
  , NP.Branch 100 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Plus (NP.Id "PyFloat_Type") (NP.Num 0))) (NP.Num 0)) (NP.Num 1)) 102 102
  , NP.Seq 101 42
  , NP.Seq 101 103
  , NP.Var 102 "NOP_102"
  , NP.Seq 102 103
  , NP.Var 103 "IF_ELSE_FOOTER"
  , NP.Assign 104 "left" (NP.Num 0)
  , NP.Seq 104 105
  , NP.Var 105 "left_o"
  , NP.Seq 105 106
  , NP.Branch 106 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Plus (NP.Id "PyFloat_Type") (NP.Num 0))) (NP.Num 0)) (NP.Num 1)) 108 108
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
  , NP.Branch 115 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 117 117
  , NP.Seq 116 1813
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
  , NP.Branch 124 (NP.Eq (NP.Num 0) (NP.Num 1)) 125 128
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
  , NP.Branch 129 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 131 170
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
  , NP.Branch 142 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 144 144
  , NP.Seq 143 42
  , NP.Seq 143 145
  , NP.Var 144 "NOP_144"
  , NP.Seq 144 145
  , NP.Var 145 "IF_ELSE_FOOTER"
  , NP.Assign 146 "left" (NP.Num 0)
  , NP.Seq 146 147
  , NP.Var 147 "left_o"
  , NP.Seq 147 148
  , NP.Branch 148 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 150 150
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
  , NP.Branch 156 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 158 158
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
  , NP.Branch 165 (NP.Eq (NP.Num 0) (NP.Num 1)) 166 169
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
  , NP.Branch 170 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 172 214
  , NP.Var 172 "NOP_172"
  , NP.Var 173 "__CLABEL_TARGET_BINARY_OP_ADD_UNICODE"
  , NP.Seq 173 174
  , NP.Var 174 "this_instr"
  , NP.Seq 174 175
  , NP.Assign 175 "undefed" (NP.Num 0)
  , NP.Seq 175 176
  , NP.Assign 176 "next_instr" (NP.Num 0)
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
  , NP.Assign 182 "value" (NP.Num 0)
  , NP.Seq 182 183
  , NP.Var 183 "value_o"
  , NP.Seq 183 184
  , NP.Branch 184 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Plus (NP.Id "PyUnicode_Type") (NP.Num 0))) (NP.Num 0)) (NP.Num 1)) 186 186
  , NP.Seq 185 42
  , NP.Seq 185 187
  , NP.Var 186 "NOP_186"
  , NP.Seq 186 187
  , NP.Var 187 "IF_ELSE_FOOTER"
  , NP.Assign 188 "nos" (NP.Num 0)
  , NP.Seq 188 189
  , NP.Var 189 "o"
  , NP.Seq 189 190
  , NP.Branch 190 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Plus (NP.Id "PyUnicode_Type") (NP.Num 0))) (NP.Num 0)) (NP.Num 1)) 192 192
  , NP.Seq 191 42
  , NP.Seq 191 193
  , NP.Var 192 "NOP_192"
  , NP.Seq 192 193
  , NP.Var 193 "IF_ELSE_FOOTER"
  , NP.Assign 194 "right" (NP.Num 0)
  , NP.Seq 194 195
  , NP.Assign 195 "left" (NP.Num 0)
  , NP.Seq 195 196
  , NP.Var 196 "left_o"
  , NP.Seq 196 197
  , NP.Var 197 "right_o"
  , NP.Seq 197 198
  , NP.Var 198 "res_o"
  , NP.Seq 198 199
  , NP.Branch 199 (NP.Eq (NP.Plus (NP.Id "res_o") (NP.Num 0)) (NP.Num 1)) 201 201
  , NP.Seq 200 1813
  , NP.Seq 200 202
  , NP.Var 201 "NOP_201"
  , NP.Seq 201 202
  , NP.Var 202 "IF_ELSE_FOOTER"
  , NP.Assign 203 "res" (NP.Num 0)
  , NP.Seq 203 204
  , NP.Assign 204 "undefed" (NP.Num 0)
  , NP.Seq 204 205
  , NP.Assign 205 "stack_pointer" (NP.Num 0)
  , NP.Seq 205 206
  , NP.Var 206 "word"
  , NP.Seq 206 207
  , NP.Assign 207 "opcode" (NP.Num 0)
  , NP.Seq 207 208
  , NP.Assign 208 "oparg" (NP.Num 0)
  , NP.Seq 208 209
  , NP.Branch 209 (NP.Eq (NP.Num 0) (NP.Num 1)) 210 213
  , NP.Var 210 "word"
  , NP.Seq 210 211
  , NP.Assign 211 "opcode" (NP.Num 0)
  , NP.Seq 211 212
  , NP.Assign 212 "oparg" (NP.Num 0)
  , NP.Seq 212 213
  , NP.Seq 212 209
  , NP.Var 213 "LOOP_FOOTER"
  , NP.Seq 213 214
  , NP.Seq 213 35
  , NP.Branch 214 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 216 260
  , NP.Var 216 "NOP_216"
  , NP.Var 217 "__CLABEL_TARGET_BINARY_OP_EXTEND"
  , NP.Seq 217 218
  , NP.Var 218 "this_instr"
  , NP.Seq 218 219
  , NP.Assign 219 "undefed" (NP.Num 0)
  , NP.Seq 219 220
  , NP.Assign 220 "next_instr" (NP.Num 0)
  , NP.Seq 220 221
  , NP.Var 221 "left"
  , NP.Seq 221 222
  , NP.Var 222 "right"
  , NP.Seq 222 223
  , NP.Var 223 "res"
  , NP.Seq 223 224
  , NP.Assign 224 "right" (NP.Num 0)
  , NP.Seq 224 225
  , NP.Assign 225 "left" (NP.Num 0)
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
  , NP.Assign 231 "stack_pointer" (NP.Num 0)
  , NP.Seq 231 232
  , NP.Branch 232 (NP.Eq (NP.Plus (NP.Id "res") (NP.Num 0)) (NP.Num 1)) 234 234
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
  , NP.Assign 242 "right" (NP.Num 0)
  , NP.Seq 242 243
  , NP.Assign 243 "undefed" (NP.Num 0)
  , NP.Seq 243 244
  , NP.Assign 244 "tmp" (NP.Num 0)
  , NP.Seq 244 245
  , NP.Assign 245 "left" (NP.Num 0)
  , NP.Seq 245 246
  , NP.Assign 246 "undefed" (NP.Num 0)
  , NP.Seq 246 247
  , NP.Assign 247 "stack_pointer" (NP.Num 0)
  , NP.Seq 247 248
  , NP.Assign 248 "stack_pointer" (NP.Num 0)
  , NP.Seq 248 249
  , NP.Assign 249 "res" (NP.Num 0)
  , NP.Seq 249 250
  , NP.Assign 250 "undefed" (NP.Num 0)
  , NP.Seq 250 251
  , NP.Assign 251 "stack_pointer" (NP.Num 0)
  , NP.Seq 251 252
  , NP.Var 252 "word"
  , NP.Seq 252 253
  , NP.Assign 253 "opcode" (NP.Num 0)
  , NP.Seq 253 254
  , NP.Assign 254 "oparg" (NP.Num 0)
  , NP.Seq 254 255
  , NP.Branch 255 (NP.Eq (NP.Num 0) (NP.Num 1)) 256 259
  , NP.Var 256 "word"
  , NP.Seq 256 257
  , NP.Assign 257 "opcode" (NP.Num 0)
  , NP.Seq 257 258
  , NP.Assign 258 "oparg" (NP.Num 0)
  , NP.Seq 258 259
  , NP.Seq 258 255
  , NP.Var 259 "LOOP_FOOTER"
  , NP.Seq 259 260
  , NP.Seq 259 35
  , NP.Branch 260 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 262 354
  , NP.Var 262 "NOP_262"
  , NP.Var 263 "__CLABEL_TARGET_BINARY_OP_INPLACE_ADD_UNICODE"
  , NP.Seq 263 264
  , NP.Var 264 "this_instr"
  , NP.Seq 264 265
  , NP.Assign 265 "undefed" (NP.Num 0)
  , NP.Seq 265 266
  , NP.Assign 266 "next_instr" (NP.Num 0)
  , NP.Seq 266 267
  , NP.Var 267 "value"
  , NP.Seq 267 268
  , NP.Var 268 "nos"
  , NP.Seq 268 269
  , NP.Var 269 "left"
  , NP.Seq 269 270
  , NP.Var 270 "right"
  , NP.Seq 270 271
  , NP.Assign 271 "value" (NP.Num 0)
  , NP.Seq 271 272
  , NP.Var 272 "value_o"
  , NP.Seq 272 273
  , NP.Branch 273 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Plus (NP.Id "PyUnicode_Type") (NP.Num 0))) (NP.Num 0)) (NP.Num 1)) 275 275
  , NP.Seq 274 42
  , NP.Seq 274 276
  , NP.Var 275 "NOP_275"
  , NP.Seq 275 276
  , NP.Var 276 "IF_ELSE_FOOTER"
  , NP.Assign 277 "nos" (NP.Num 0)
  , NP.Seq 277 278
  , NP.Var 278 "o"
  , NP.Seq 278 279
  , NP.Branch 279 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Plus (NP.Id "PyUnicode_Type") (NP.Num 0))) (NP.Num 0)) (NP.Num 1)) 281 281
  , NP.Seq 280 42
  , NP.Seq 280 282
  , NP.Var 281 "NOP_281"
  , NP.Seq 281 282
  , NP.Var 282 "IF_ELSE_FOOTER"
  , NP.Assign 283 "right" (NP.Num 0)
  , NP.Seq 283 284
  , NP.Assign 284 "left" (NP.Num 0)
  , NP.Seq 284 285
  , NP.Var 285 "left_o"
  , NP.Seq 285 286
  , NP.Var 286 "next_oparg"
  , NP.Seq 286 287
  , NP.Assign 287 "next_oparg" (NP.Num 0)
  , NP.Seq 287 288
  , NP.Var 288 "target_local"
  , NP.Seq 288 289
  , NP.Branch 289 (NP.Eq (NP.Plus (NP.Num 0) (NP.Id "left_o")) (NP.Num 1)) 291 291
  , NP.Seq 290 42
  , NP.Seq 290 292
  , NP.Var 291 "NOP_291"
  , NP.Seq 291 292
  , NP.Var 292 "IF_ELSE_FOOTER"
  , NP.Var 293 "temp"
  , NP.Seq 293 294
  , NP.Var 294 "right_o"
  , NP.Seq 294 295
  , NP.Assign 295 "stack_pointer" (NP.Num 0)
  , NP.Seq 295 296
  , NP.Assign 296 "stack_pointer" (NP.Num 0)
  , NP.Seq 296 297
  , NP.Assign 297 "undefed" (NP.Num 0)
  , NP.Seq 297 298
  , NP.Var 298 "op"
  , NP.Seq 298 299
  , NP.Branch 299 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 0)) (NP.Num 1)) 301 316
  , NP.Var 301 "tracer"
  , NP.Seq 301 302
  , NP.Branch 302 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 304 305
  , NP.Var 304 "data"
  , NP.Seq 304 305
  , NP.Seq 304 306
  , NP.Var 305 "NOP_305"
  , NP.Seq 305 306
  , NP.Var 306 "IF_ELSE_FOOTER"
  , NP.Branch 307 (NP.Eq (NP.Num 0) (NP.Num 1)) 308 314
  , NP.Var 308 "tracer"
  , NP.Seq 308 309
  , NP.Branch 309 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 311 312
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
  , NP.Branch 318 (NP.Eq (NP.Num 0) (NP.Num 1)) 319 339
  , NP.Var 319 "op"
  , NP.Seq 319 320
  , NP.Branch 320 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 0)) (NP.Num 1)) 322 337
  , NP.Var 322 "tracer"
  , NP.Seq 322 323
  , NP.Branch 323 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 325 326
  , NP.Var 325 "data"
  , NP.Seq 325 326
  , NP.Seq 325 327
  , NP.Var 326 "NOP_326"
  , NP.Seq 326 327
  , NP.Var 327 "IF_ELSE_FOOTER"
  , NP.Branch 328 (NP.Eq (NP.Num 0) (NP.Num 1)) 329 335
  , NP.Var 329 "tracer"
  , NP.Seq 329 330
  , NP.Branch 330 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 332 333
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
  , NP.Assign 340 "stack_pointer" (NP.Num 0)
  , NP.Seq 340 341
  , NP.Branch 341 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 343 343
  , NP.Seq 342 1817
  , NP.Seq 342 344
  , NP.Var 343 "NOP_343"
  , NP.Seq 343 344
  , NP.Var 344 "IF_ELSE_FOOTER"
  , NP.Assign 345 "next_instr" (NP.Num 0)
  , NP.Seq 345 346
  , NP.Var 346 "word"
  , NP.Seq 346 347
  , NP.Assign 347 "opcode" (NP.Num 0)
  , NP.Seq 347 348
  , NP.Assign 348 "oparg" (NP.Num 0)
  , NP.Seq 348 349
  , NP.Branch 349 (NP.Eq (NP.Num 0) (NP.Num 1)) 350 353
  , NP.Var 350 "word"
  , NP.Seq 350 351
  , NP.Assign 351 "opcode" (NP.Num 0)
  , NP.Seq 351 352
  , NP.Assign 352 "oparg" (NP.Num 0)
  , NP.Seq 352 353
  , NP.Seq 352 349
  , NP.Var 353 "LOOP_FOOTER"
  , NP.Seq 353 354
  , NP.Seq 353 35
  , NP.Branch 354 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 356 396
  , NP.Var 356 "NOP_356"
  , NP.Var 357 "__CLABEL_TARGET_BINARY_OP_MULTIPLY_FLOAT"
  , NP.Seq 357 358
  , NP.Var 358 "this_instr"
  , NP.Seq 358 359
  , NP.Assign 359 "undefed" (NP.Num 0)
  , NP.Seq 359 360
  , NP.Assign 360 "next_instr" (NP.Num 0)
  , NP.Seq 360 361
  , NP.Var 361 "value"
  , NP.Seq 361 362
  , NP.Var 362 "left"
  , NP.Seq 362 363
  , NP.Var 363 "right"
  , NP.Seq 363 364
  , NP.Var 364 "res"
  , NP.Seq 364 365
  , NP.Assign 365 "value" (NP.Num 0)
  , NP.Seq 365 366
  , NP.Var 366 "value_o"
  , NP.Seq 366 367
  , NP.Branch 367 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Plus (NP.Id "PyFloat_Type") (NP.Num 0))) (NP.Num 0)) (NP.Num 1)) 369 369
  , NP.Seq 368 42
  , NP.Seq 368 370
  , NP.Var 369 "NOP_369"
  , NP.Seq 369 370
  , NP.Var 370 "IF_ELSE_FOOTER"
  , NP.Assign 371 "left" (NP.Num 0)
  , NP.Seq 371 372
  , NP.Var 372 "left_o"
  , NP.Seq 372 373
  , NP.Branch 373 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Plus (NP.Id "PyFloat_Type") (NP.Num 0))) (NP.Num 0)) (NP.Num 1)) 375 375
  , NP.Seq 374 42
  , NP.Seq 374 376
  , NP.Var 375 "NOP_375"
  , NP.Seq 375 376
  , NP.Var 376 "IF_ELSE_FOOTER"
  , NP.Assign 377 "right" (NP.Num 0)
  , NP.Seq 377 378
  , NP.Var 378 "left_o"
  , NP.Seq 378 379
  , NP.Var 379 "right_o"
  , NP.Seq 379 380
  , NP.Var 380 "dres"
  , NP.Seq 380 381
  , NP.Assign 381 "res" (NP.Num 0)
  , NP.Seq 381 382
  , NP.Branch 382 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 384 384
  , NP.Seq 383 1813
  , NP.Seq 383 385
  , NP.Var 384 "NOP_384"
  , NP.Seq 384 385
  , NP.Var 385 "IF_ELSE_FOOTER"
  , NP.Assign 386 "undefed" (NP.Num 0)
  , NP.Seq 386 387
  , NP.Assign 387 "stack_pointer" (NP.Num 0)
  , NP.Seq 387 388
  , NP.Var 388 "word"
  , NP.Seq 388 389
  , NP.Assign 389 "opcode" (NP.Num 0)
  , NP.Seq 389 390
  , NP.Assign 390 "oparg" (NP.Num 0)
  , NP.Seq 390 391
  , NP.Branch 391 (NP.Eq (NP.Num 0) (NP.Num 1)) 392 395
  , NP.Var 392 "word"
  , NP.Seq 392 393
  , NP.Assign 393 "opcode" (NP.Num 0)
  , NP.Seq 393 394
  , NP.Assign 394 "oparg" (NP.Num 0)
  , NP.Seq 394 395
  , NP.Seq 394 391
  , NP.Var 395 "LOOP_FOOTER"
  , NP.Seq 395 396
  , NP.Seq 395 35
  , NP.Branch 396 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 398 437
  , NP.Var 398 "NOP_398"
  , NP.Var 399 "__CLABEL_TARGET_BINARY_OP_MULTIPLY_INT"
  , NP.Seq 399 400
  , NP.Var 400 "this_instr"
  , NP.Seq 400 401
  , NP.Assign 401 "undefed" (NP.Num 0)
  , NP.Seq 401 402
  , NP.Assign 402 "next_instr" (NP.Num 0)
  , NP.Seq 402 403
  , NP.Var 403 "value"
  , NP.Seq 403 404
  , NP.Var 404 "left"
  , NP.Seq 404 405
  , NP.Var 405 "right"
  , NP.Seq 405 406
  , NP.Var 406 "res"
  , NP.Seq 406 407
  , NP.Assign 407 "value" (NP.Num 0)
  , NP.Seq 407 408
  , NP.Var 408 "value_o"
  , NP.Seq 408 409
  , NP.Branch 409 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 411 411
  , NP.Seq 410 42
  , NP.Seq 410 412
  , NP.Var 411 "NOP_411"
  , NP.Seq 411 412
  , NP.Var 412 "IF_ELSE_FOOTER"
  , NP.Assign 413 "left" (NP.Num 0)
  , NP.Seq 413 414
  , NP.Var 414 "left_o"
  , NP.Seq 414 415
  , NP.Branch 415 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 417 417
  , NP.Seq 416 42
  , NP.Seq 416 418
  , NP.Var 417 "NOP_417"
  , NP.Seq 417 418
  , NP.Var 418 "IF_ELSE_FOOTER"
  , NP.Assign 419 "right" (NP.Num 0)
  , NP.Seq 419 420
  , NP.Var 420 "left_o"
  , NP.Seq 420 421
  , NP.Var 421 "right_o"
  , NP.Seq 421 422
  , NP.Assign 422 "res" (NP.Num 0)
  , NP.Seq 422 423
  , NP.Branch 423 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 425 425
  , NP.Seq 424 42
  , NP.Seq 424 426
  , NP.Var 425 "NOP_425"
  , NP.Seq 425 426
  , NP.Var 426 "IF_ELSE_FOOTER"
  , NP.Assign 427 "undefed" (NP.Num 0)
  , NP.Seq 427 428
  , NP.Assign 428 "stack_pointer" (NP.Num 0)
  , NP.Seq 428 429
  , NP.Var 429 "word"
  , NP.Seq 429 430
  , NP.Assign 430 "opcode" (NP.Num 0)
  , NP.Seq 430 431
  , NP.Assign 431 "oparg" (NP.Num 0)
  , NP.Seq 431 432
  , NP.Branch 432 (NP.Eq (NP.Num 0) (NP.Num 1)) 433 436
  , NP.Var 433 "word"
  , NP.Seq 433 434
  , NP.Assign 434 "opcode" (NP.Num 0)
  , NP.Seq 434 435
  , NP.Assign 435 "oparg" (NP.Num 0)
  , NP.Seq 435 436
  , NP.Seq 435 432
  , NP.Var 436 "LOOP_FOOTER"
  , NP.Seq 436 437
  , NP.Seq 436 35
  , NP.Branch 437 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 439 489
  , NP.Var 439 "NOP_439"
  , NP.Var 440 "__CLABEL_TARGET_BINARY_OP_SUBSCR_DICT"
  , NP.Seq 440 441
  , NP.Var 441 "this_instr"
  , NP.Seq 441 442
  , NP.Assign 442 "undefed" (NP.Num 0)
  , NP.Seq 442 443
  , NP.Assign 443 "next_instr" (NP.Num 0)
  , NP.Seq 443 444
  , NP.Var 444 "nos"
  , NP.Seq 444 445
  , NP.Var 445 "dict_st"
  , NP.Seq 445 446
  , NP.Var 446 "sub_st"
  , NP.Seq 446 447
  , NP.Var 447 "res"
  , NP.Seq 447 448
  , NP.Assign 448 "nos" (NP.Num 0)
  , NP.Seq 448 449
  , NP.Var 449 "o"
  , NP.Seq 449 450
  , NP.Branch 450 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Plus (NP.Id "PyDict_Type") (NP.Num 0))) (NP.Num 0)) (NP.Num 1)) 452 452
  , NP.Seq 451 42
  , NP.Seq 451 453
  , NP.Var 452 "NOP_452"
  , NP.Seq 452 453
  , NP.Var 453 "IF_ELSE_FOOTER"
  , NP.Assign 454 "sub_st" (NP.Num 0)
  , NP.Seq 454 455
  , NP.Assign 455 "dict_st" (NP.Num 0)
  , NP.Seq 455 456
  , NP.Var 456 "sub"
  , NP.Seq 456 457
  , NP.Var 457 "dict"
  , NP.Seq 457 458
  , NP.Var 458 "res_o"
  , NP.Seq 458 459
  , NP.Var 459 "rc"
  , NP.Seq 459 460
  , NP.Assign 460 "stack_pointer" (NP.Num 0)
  , NP.Seq 460 461
  , NP.Branch 461 (NP.Eq (NP.Plus (NP.Id "rc") (NP.Num 0)) (NP.Num 1)) 463 464
  , NP.Assign 463 "stack_pointer" (NP.Num 0)
  , NP.Seq 463 465
  , NP.Var 464 "NOP_464"
  , NP.Seq 464 465
  , NP.Var 465 "IF_ELSE_FOOTER"
  , NP.Var 466 "tmp"
  , NP.Seq 466 467
  , NP.Assign 467 "sub_st" (NP.Num 0)
  , NP.Seq 467 468
  , NP.Assign 468 "undefed" (NP.Num 0)
  , NP.Seq 468 469
  , NP.Assign 469 "tmp" (NP.Num 0)
  , NP.Seq 469 470
  , NP.Assign 470 "dict_st" (NP.Num 0)
  , NP.Seq 470 471
  , NP.Assign 471 "undefed" (NP.Num 0)
  , NP.Seq 471 472
  , NP.Assign 472 "stack_pointer" (NP.Num 0)
  , NP.Seq 472 473
  , NP.Assign 473 "stack_pointer" (NP.Num 0)
  , NP.Seq 473 474
  , NP.Branch 474 (NP.Eq (NP.Plus (NP.Id "rc") (NP.Num 0)) (NP.Num 1)) 476 476
  , NP.Seq 475 1817
  , NP.Seq 475 477
  , NP.Var 476 "NOP_476"
  , NP.Seq 476 477
  , NP.Var 477 "IF_ELSE_FOOTER"
  , NP.Assign 478 "res" (NP.Num 0)
  , NP.Seq 478 479
  , NP.Assign 479 "undefed" (NP.Num 0)
  , NP.Seq 479 480
  , NP.Assign 480 "stack_pointer" (NP.Num 0)
  , NP.Seq 480 481
  , NP.Var 481 "word"
  , NP.Seq 481 482
  , NP.Assign 482 "opcode" (NP.Num 0)
  , NP.Seq 482 483
  , NP.Assign 483 "oparg" (NP.Num 0)
  , NP.Seq 483 484
  , NP.Branch 484 (NP.Eq (NP.Num 0) (NP.Num 1)) 485 488
  , NP.Var 485 "word"
  , NP.Seq 485 486
  , NP.Assign 486 "opcode" (NP.Num 0)
  , NP.Seq 486 487
  , NP.Assign 487 "oparg" (NP.Num 0)
  , NP.Seq 487 488
  , NP.Seq 487 484
  , NP.Var 488 "LOOP_FOOTER"
  , NP.Seq 488 489
  , NP.Seq 488 35
  , NP.Branch 489 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 491 549
  , NP.Var 491 "NOP_491"
  , NP.Var 492 "__CLABEL_TARGET_BINARY_OP_SUBSCR_GETITEM"
  , NP.Seq 492 493
  , NP.Var 493 "this_instr"
  , NP.Seq 493 494
  , NP.Assign 494 "undefed" (NP.Num 0)
  , NP.Seq 494 495
  , NP.Assign 495 "next_instr" (NP.Num 0)
  , NP.Seq 495 496
  , NP.Var 496 "container"
  , NP.Seq 496 497
  , NP.Var 497 "getitem"
  , NP.Seq 497 498
  , NP.Var 498 "sub"
  , NP.Seq 498 499
  , NP.Var 499 "new_frame"
  , NP.Seq 499 500
  , NP.Branch 500 (NP.Eq (NP.Num 0) (NP.Num 1)) 502 502
  , NP.Seq 501 42
  , NP.Seq 501 503
  , NP.Var 502 "NOP_502"
  , NP.Seq 502 503
  , NP.Var 503 "IF_ELSE_FOOTER"
  , NP.Assign 504 "container" (NP.Num 0)
  , NP.Seq 504 505
  , NP.Var 505 "tp"
  , NP.Seq 505 506
  , NP.Branch 506 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 508 508
  , NP.Seq 507 42
  , NP.Seq 507 509
  , NP.Var 508 "NOP_508"
  , NP.Seq 508 509
  , NP.Var 509 "IF_ELSE_FOOTER"
  , NP.Var 510 "ht"
  , NP.Seq 510 511
  , NP.Var 511 "getitem_o"
  , NP.Seq 511 512
  , NP.Branch 512 (NP.Eq (NP.Plus (NP.Id "getitem_o") (NP.Num 0)) (NP.Num 1)) 514 514
  , NP.Seq 513 42
  , NP.Seq 513 515
  , NP.Var 514 "NOP_514"
  , NP.Seq 514 515
  , NP.Var 515 "IF_ELSE_FOOTER"
  , NP.Var 516 "cached_version"
  , NP.Seq 516 517
  , NP.Branch 517 (NP.Eq (NP.Plus (NP.Num 0) (NP.Id "cached_version")) (NP.Num 1)) 519 519
  , NP.Seq 518 42
  , NP.Seq 518 520
  , NP.Var 519 "NOP_519"
  , NP.Seq 519 520
  , NP.Var 520 "IF_ELSE_FOOTER"
  , NP.Var 521 "code"
  , NP.Seq 521 522
  , NP.Branch 522 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 524 524
  , NP.Seq 523 42
  , NP.Seq 523 525
  , NP.Var 524 "NOP_524"
  , NP.Seq 524 525
  , NP.Var 525 "IF_ELSE_FOOTER"
  , NP.Assign 526 "getitem" (NP.Num 0)
  , NP.Seq 526 527
  , NP.Assign 527 "sub" (NP.Num 0)
  , NP.Seq 527 528
  , NP.Var 528 "pushed_frame"
  , NP.Seq 528 529
  , NP.Assign 529 "undefed" (NP.Num 0)
  , NP.Seq 529 530
  , NP.Assign 530 "undefed" (NP.Num 0)
  , NP.Seq 530 531
  , NP.Assign 531 "undefed" (NP.Num 0)
  , NP.Seq 531 532
  , NP.Assign 532 "new_frame" (NP.Num 0)
  , NP.Seq 532 533
  , NP.Var 533 "temp"
  , NP.Seq 533 534
  , NP.Assign 534 "stack_pointer" (NP.Num 0)
  , NP.Seq 534 535
  , NP.Assign 535 "frame" (NP.Num 0)
  , NP.Seq 535 536
  , NP.Assign 536 "stack_pointer" (NP.Num 0)
  , NP.Seq 536 537
  , NP.Assign 537 "next_instr" (NP.Num 0)
  , NP.Seq 537 538
  , NP.Branch 538 (NP.Eq (NP.Num 0) (NP.Num 1)) 539 540
  , NP.Assign 539 "next_instr" (NP.Num 0)
  , NP.Seq 539 538
  , NP.Var 540 "LOOP_FOOTER"
  , NP.Seq 540 541
  , NP.Var 541 "word"
  , NP.Seq 541 542
  , NP.Assign 542 "opcode" (NP.Num 0)
  , NP.Seq 542 543
  , NP.Assign 543 "oparg" (NP.Num 0)
  , NP.Seq 543 544
  , NP.Branch 544 (NP.Eq (NP.Num 0) (NP.Num 1)) 545 548
  , NP.Var 545 "word"
  , NP.Seq 545 546
  , NP.Assign 546 "opcode" (NP.Num 0)
  , NP.Seq 546 547
  , NP.Assign 547 "oparg" (NP.Num 0)
  , NP.Seq 547 548
  , NP.Seq 547 544
  , NP.Var 548 "LOOP_FOOTER"
  , NP.Seq 548 549
  , NP.Seq 548 35
  , NP.Branch 549 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 551 604
  , NP.Var 551 "NOP_551"
  , NP.Var 552 "__CLABEL_TARGET_BINARY_OP_SUBSCR_LIST_INT"
  , NP.Seq 552 553
  , NP.Var 553 "this_instr"
  , NP.Seq 553 554
  , NP.Assign 554 "undefed" (NP.Num 0)
  , NP.Seq 554 555
  , NP.Assign 555 "next_instr" (NP.Num 0)
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
  , NP.Assign 561 "value" (NP.Num 0)
  , NP.Seq 561 562
  , NP.Var 562 "value_o"
  , NP.Seq 562 563
  , NP.Branch 563 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 565 565
  , NP.Seq 564 42
  , NP.Seq 564 566
  , NP.Var 565 "NOP_565"
  , NP.Seq 565 566
  , NP.Var 566 "IF_ELSE_FOOTER"
  , NP.Assign 567 "nos" (NP.Num 0)
  , NP.Seq 567 568
  , NP.Var 568 "o"
  , NP.Seq 568 569
  , NP.Branch 569 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Plus (NP.Id "PyList_Type") (NP.Num 0))) (NP.Num 0)) (NP.Num 1)) 571 571
  , NP.Seq 570 42
  , NP.Seq 570 572
  , NP.Var 571 "NOP_571"
  , NP.Seq 571 572
  , NP.Var 572 "IF_ELSE_FOOTER"
  , NP.Assign 573 "sub_st" (NP.Num 0)
  , NP.Seq 573 574
  , NP.Assign 574 "list_st" (NP.Num 0)
  , NP.Seq 574 575
  , NP.Var 575 "sub"
  , NP.Seq 575 576
  , NP.Var 576 "list"
  , NP.Seq 576 577
  , NP.Branch 577 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 579 579
  , NP.Seq 578 42
  , NP.Seq 578 580
  , NP.Var 579 "NOP_579"
  , NP.Seq 579 580
  , NP.Var 580 "IF_ELSE_FOOTER"
  , NP.Var 581 "index"
  , NP.Seq 581 582
  , NP.Branch 582 (NP.Eq (NP.Plus (NP.Id "index") (NP.Num 0)) (NP.Num 1)) 584 584
  , NP.Seq 583 42
  , NP.Seq 583 585
  , NP.Var 584 "NOP_584"
  , NP.Seq 584 585
  , NP.Var 585 "IF_ELSE_FOOTER"
  , NP.Var 586 "res_o"
  , NP.Seq 586 587
  , NP.Assign 587 "res" (NP.Num 0)
  , NP.Seq 587 588
  , NP.Var 588 "tmp"
  , NP.Seq 588 589
  , NP.Assign 589 "list_st" (NP.Num 0)
  , NP.Seq 589 590
  , NP.Assign 590 "undefed" (NP.Num 0)
  , NP.Seq 590 591
  , NP.Assign 591 "tmp" (NP.Num 0)
  , NP.Seq 591 592
  , NP.Assign 592 "sub_st" (NP.Num 0)
  , NP.Seq 592 593
  , NP.Assign 593 "undefed" (NP.Num 0)
  , NP.Seq 593 594
  , NP.Assign 594 "stack_pointer" (NP.Num 0)
  , NP.Seq 594 595
  , NP.Assign 595 "stack_pointer" (NP.Num 0)
  , NP.Seq 595 596
  , NP.Var 596 "word"
  , NP.Seq 596 597
  , NP.Assign 597 "opcode" (NP.Num 0)
  , NP.Seq 597 598
  , NP.Assign 598 "oparg" (NP.Num 0)
  , NP.Seq 598 599
  , NP.Branch 599 (NP.Eq (NP.Num 0) (NP.Num 1)) 600 603
  , NP.Var 600 "word"
  , NP.Seq 600 601
  , NP.Assign 601 "opcode" (NP.Num 0)
  , NP.Seq 601 602
  , NP.Assign 602 "oparg" (NP.Num 0)
  , NP.Seq 602 603
  , NP.Seq 602 599
  , NP.Var 603 "LOOP_FOOTER"
  , NP.Seq 603 604
  , NP.Seq 603 35
  , NP.Branch 604 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 606 657
  , NP.Var 606 "NOP_606"
  , NP.Var 607 "__CLABEL_TARGET_BINARY_OP_SUBSCR_LIST_SLICE"
  , NP.Seq 607 608
  , NP.Var 608 "this_instr"
  , NP.Seq 608 609
  , NP.Assign 609 "undefed" (NP.Num 0)
  , NP.Seq 609 610
  , NP.Assign 610 "next_instr" (NP.Num 0)
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
  , NP.Assign 616 "tos" (NP.Num 0)
  , NP.Seq 616 617
  , NP.Var 617 "o"
  , NP.Seq 617 618
  , NP.Branch 618 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Plus (NP.Id "PySlice_Type") (NP.Num 0))) (NP.Num 0)) (NP.Num 1)) 620 620
  , NP.Seq 619 42
  , NP.Seq 619 621
  , NP.Var 620 "NOP_620"
  , NP.Seq 620 621
  , NP.Var 621 "IF_ELSE_FOOTER"
  , NP.Assign 622 "nos" (NP.Num 0)
  , NP.Seq 622 623
  , NP.Var 623 "o"
  , NP.Seq 623 624
  , NP.Branch 624 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Plus (NP.Id "PyList_Type") (NP.Num 0))) (NP.Num 0)) (NP.Num 1)) 626 626
  , NP.Seq 625 42
  , NP.Seq 625 627
  , NP.Var 626 "NOP_626"
  , NP.Seq 626 627
  , NP.Var 627 "IF_ELSE_FOOTER"
  , NP.Assign 628 "sub_st" (NP.Num 0)
  , NP.Seq 628 629
  , NP.Assign 629 "list_st" (NP.Num 0)
  , NP.Seq 629 630
  , NP.Var 630 "sub"
  , NP.Seq 630 631
  , NP.Var 631 "list"
  , NP.Seq 631 632
  , NP.Var 632 "res_o"
  , NP.Seq 632 633
  , NP.Assign 633 "stack_pointer" (NP.Num 0)
  , NP.Seq 633 634
  , NP.Var 634 "tmp"
  , NP.Seq 634 635
  , NP.Assign 635 "sub_st" (NP.Num 0)
  , NP.Seq 635 636
  , NP.Assign 636 "undefed" (NP.Num 0)
  , NP.Seq 636 637
  , NP.Assign 637 "tmp" (NP.Num 0)
  , NP.Seq 637 638
  , NP.Assign 638 "list_st" (NP.Num 0)
  , NP.Seq 638 639
  , NP.Assign 639 "undefed" (NP.Num 0)
  , NP.Seq 639 640
  , NP.Assign 640 "stack_pointer" (NP.Num 0)
  , NP.Seq 640 641
  , NP.Assign 641 "stack_pointer" (NP.Num 0)
  , NP.Seq 641 642
  , NP.Branch 642 (NP.Eq (NP.Plus (NP.Id "res_o") (NP.Num 0)) (NP.Num 1)) 644 644
  , NP.Seq 643 1817
  , NP.Seq 643 645
  , NP.Var 644 "NOP_644"
  , NP.Seq 644 645
  , NP.Var 645 "IF_ELSE_FOOTER"
  , NP.Assign 646 "res" (NP.Num 0)
  , NP.Seq 646 647
  , NP.Assign 647 "undefed" (NP.Num 0)
  , NP.Seq 647 648
  , NP.Assign 648 "stack_pointer" (NP.Num 0)
  , NP.Seq 648 649
  , NP.Var 649 "word"
  , NP.Seq 649 650
  , NP.Assign 650 "opcode" (NP.Num 0)
  , NP.Seq 650 651
  , NP.Assign 651 "oparg" (NP.Num 0)
  , NP.Seq 651 652
  , NP.Branch 652 (NP.Eq (NP.Num 0) (NP.Num 1)) 653 656
  , NP.Var 653 "word"
  , NP.Seq 653 654
  , NP.Assign 654 "opcode" (NP.Num 0)
  , NP.Seq 654 655
  , NP.Assign 655 "oparg" (NP.Num 0)
  , NP.Seq 655 656
  , NP.Seq 655 652
  , NP.Var 656 "LOOP_FOOTER"
  , NP.Seq 656 657
  , NP.Seq 656 35
  , NP.Branch 657 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 659 713
  , NP.Var 659 "NOP_659"
  , NP.Var 660 "__CLABEL_TARGET_BINARY_OP_SUBSCR_STR_INT"
  , NP.Seq 660 661
  , NP.Var 661 "this_instr"
  , NP.Seq 661 662
  , NP.Assign 662 "undefed" (NP.Num 0)
  , NP.Seq 662 663
  , NP.Assign 663 "next_instr" (NP.Num 0)
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
  , NP.Assign 669 "value" (NP.Num 0)
  , NP.Seq 669 670
  , NP.Var 670 "value_o"
  , NP.Seq 670 671
  , NP.Branch 671 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 673 673
  , NP.Seq 672 42
  , NP.Seq 672 674
  , NP.Var 673 "NOP_673"
  , NP.Seq 673 674
  , NP.Var 674 "IF_ELSE_FOOTER"
  , NP.Assign 675 "nos" (NP.Num 0)
  , NP.Seq 675 676
  , NP.Var 676 "o"
  , NP.Seq 676 677
  , NP.Branch 677 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Plus (NP.Id "PyUnicode_Type") (NP.Num 0))) (NP.Num 0)) (NP.Num 1)) 679 679
  , NP.Seq 678 42
  , NP.Seq 678 680
  , NP.Var 679 "NOP_679"
  , NP.Seq 679 680
  , NP.Var 680 "IF_ELSE_FOOTER"
  , NP.Assign 681 "sub_st" (NP.Num 0)
  , NP.Seq 681 682
  , NP.Assign 682 "str_st" (NP.Num 0)
  , NP.Seq 682 683
  , NP.Var 683 "sub"
  , NP.Seq 683 684
  , NP.Var 684 "str"
  , NP.Seq 684 685
  , NP.Branch 685 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 687 687
  , NP.Seq 686 42
  , NP.Seq 686 688
  , NP.Var 687 "NOP_687"
  , NP.Seq 687 688
  , NP.Var 688 "IF_ELSE_FOOTER"
  , NP.Var 689 "index"
  , NP.Seq 689 690
  , NP.Branch 690 (NP.Eq (NP.Plus (NP.Num 0) (NP.Id "index")) (NP.Num 1)) 692 692
  , NP.Seq 691 42
  , NP.Seq 691 693
  , NP.Var 692 "NOP_692"
  , NP.Seq 692 693
  , NP.Var 693 "IF_ELSE_FOOTER"
  , NP.Var 694 "c"
  , NP.Seq 694 695
  , NP.Branch 695 (NP.Eq (NP.Plus (NP.Plus (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Plus (NP.Num 0) (NP.Num 0))) (NP.Id "c")) (NP.Num 1)) 697 697
  , NP.Seq 696 42
  , NP.Seq 696 698
  , NP.Var 697 "NOP_697"
  , NP.Seq 697 698
  , NP.Var 698 "IF_ELSE_FOOTER"
  , NP.Var 699 "res_o"
  , NP.Seq 699 700
  , NP.Assign 700 "stack_pointer" (NP.Num 0)
  , NP.Seq 700 701
  , NP.Assign 701 "stack_pointer" (NP.Num 0)
  , NP.Seq 701 702
  , NP.Assign 702 "res" (NP.Num 0)
  , NP.Seq 702 703
  , NP.Assign 703 "undefed" (NP.Num 0)
  , NP.Seq 703 704
  , NP.Assign 704 "stack_pointer" (NP.Num 0)
  , NP.Seq 704 705
  , NP.Var 705 "word"
  , NP.Seq 705 706
  , NP.Assign 706 "opcode" (NP.Num 0)
  , NP.Seq 706 707
  , NP.Assign 707 "oparg" (NP.Num 0)
  , NP.Seq 707 708
  , NP.Branch 708 (NP.Eq (NP.Num 0) (NP.Num 1)) 709 712
  , NP.Var 709 "word"
  , NP.Seq 709 710
  , NP.Assign 710 "opcode" (NP.Num 0)
  , NP.Seq 710 711
  , NP.Assign 711 "oparg" (NP.Num 0)
  , NP.Seq 711 712
  , NP.Seq 711 708
  , NP.Var 712 "LOOP_FOOTER"
  , NP.Seq 712 713
  , NP.Seq 712 35
  , NP.Branch 713 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 715 765
  , NP.Var 715 "NOP_715"
  , NP.Var 716 "__CLABEL_TARGET_BINARY_OP_SUBSCR_TUPLE_INT"
  , NP.Seq 716 717
  , NP.Var 717 "this_instr"
  , NP.Seq 717 718
  , NP.Assign 718 "undefed" (NP.Num 0)
  , NP.Seq 718 719
  , NP.Assign 719 "next_instr" (NP.Num 0)
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
  , NP.Assign 725 "value" (NP.Num 0)
  , NP.Seq 725 726
  , NP.Var 726 "value_o"
  , NP.Seq 726 727
  , NP.Branch 727 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 729 729
  , NP.Seq 728 42
  , NP.Seq 728 730
  , NP.Var 729 "NOP_729"
  , NP.Seq 729 730
  , NP.Var 730 "IF_ELSE_FOOTER"
  , NP.Assign 731 "nos" (NP.Num 0)
  , NP.Seq 731 732
  , NP.Var 732 "o"
  , NP.Seq 732 733
  , NP.Branch 733 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Plus (NP.Id "PyTuple_Type") (NP.Num 0))) (NP.Num 0)) (NP.Num 1)) 735 735
  , NP.Seq 734 42
  , NP.Seq 734 736
  , NP.Var 735 "NOP_735"
  , NP.Seq 735 736
  , NP.Var 736 "IF_ELSE_FOOTER"
  , NP.Assign 737 "sub_st" (NP.Num 0)
  , NP.Seq 737 738
  , NP.Assign 738 "tuple_st" (NP.Num 0)
  , NP.Seq 738 739
  , NP.Var 739 "sub"
  , NP.Seq 739 740
  , NP.Var 740 "tuple"
  , NP.Seq 740 741
  , NP.Branch 741 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 743 743
  , NP.Seq 742 42
  , NP.Seq 742 744
  , NP.Var 743 "NOP_743"
  , NP.Seq 743 744
  , NP.Var 744 "IF_ELSE_FOOTER"
  , NP.Var 745 "index"
  , NP.Seq 745 746
  , NP.Branch 746 (NP.Eq (NP.Plus (NP.Id "index") (NP.Num 0)) (NP.Num 1)) 748 748
  , NP.Seq 747 42
  , NP.Seq 747 749
  , NP.Var 748 "NOP_748"
  , NP.Seq 748 749
  , NP.Var 749 "IF_ELSE_FOOTER"
  , NP.Var 750 "res_o"
  , NP.Seq 750 751
  , NP.Assign 751 "res" (NP.Num 0)
  , NP.Seq 751 752
  , NP.Assign 752 "stack_pointer" (NP.Num 0)
  , NP.Seq 752 753
  , NP.Var 753 "tmp"
  , NP.Seq 753 754
  , NP.Assign 754 "tuple_st" (NP.Num 0)
  , NP.Seq 754 755
  , NP.Assign 755 "undefed" (NP.Num 0)
  , NP.Seq 755 756
  , NP.Assign 756 "stack_pointer" (NP.Num 0)
  , NP.Seq 756 757
  , NP.Var 757 "word"
  , NP.Seq 757 758
  , NP.Assign 758 "opcode" (NP.Num 0)
  , NP.Seq 758 759
  , NP.Assign 759 "oparg" (NP.Num 0)
  , NP.Seq 759 760
  , NP.Branch 760 (NP.Eq (NP.Num 0) (NP.Num 1)) 761 764
  , NP.Var 761 "word"
  , NP.Seq 761 762
  , NP.Assign 762 "opcode" (NP.Num 0)
  , NP.Seq 762 763
  , NP.Assign 763 "oparg" (NP.Num 0)
  , NP.Seq 763 764
  , NP.Seq 763 760
  , NP.Var 764 "LOOP_FOOTER"
  , NP.Seq 764 765
  , NP.Seq 764 35
  , NP.Branch 765 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 767 807
  , NP.Var 767 "NOP_767"
  , NP.Var 768 "__CLABEL_TARGET_BINARY_OP_SUBTRACT_FLOAT"
  , NP.Seq 768 769
  , NP.Var 769 "this_instr"
  , NP.Seq 769 770
  , NP.Assign 770 "undefed" (NP.Num 0)
  , NP.Seq 770 771
  , NP.Assign 771 "next_instr" (NP.Num 0)
  , NP.Seq 771 772
  , NP.Var 772 "value"
  , NP.Seq 772 773
  , NP.Var 773 "left"
  , NP.Seq 773 774
  , NP.Var 774 "right"
  , NP.Seq 774 775
  , NP.Var 775 "res"
  , NP.Seq 775 776
  , NP.Assign 776 "value" (NP.Num 0)
  , NP.Seq 776 777
  , NP.Var 777 "value_o"
  , NP.Seq 777 778
  , NP.Branch 778 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Plus (NP.Id "PyFloat_Type") (NP.Num 0))) (NP.Num 0)) (NP.Num 1)) 780 780
  , NP.Seq 779 42
  , NP.Seq 779 781
  , NP.Var 780 "NOP_780"
  , NP.Seq 780 781
  , NP.Var 781 "IF_ELSE_FOOTER"
  , NP.Assign 782 "left" (NP.Num 0)
  , NP.Seq 782 783
  , NP.Var 783 "left_o"
  , NP.Seq 783 784
  , NP.Branch 784 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Plus (NP.Id "PyFloat_Type") (NP.Num 0))) (NP.Num 0)) (NP.Num 1)) 786 786
  , NP.Seq 785 42
  , NP.Seq 785 787
  , NP.Var 786 "NOP_786"
  , NP.Seq 786 787
  , NP.Var 787 "IF_ELSE_FOOTER"
  , NP.Assign 788 "right" (NP.Num 0)
  , NP.Seq 788 789
  , NP.Var 789 "left_o"
  , NP.Seq 789 790
  , NP.Var 790 "right_o"
  , NP.Seq 790 791
  , NP.Var 791 "dres"
  , NP.Seq 791 792
  , NP.Assign 792 "res" (NP.Num 0)
  , NP.Seq 792 793
  , NP.Branch 793 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 795 795
  , NP.Seq 794 1813
  , NP.Seq 794 796
  , NP.Var 795 "NOP_795"
  , NP.Seq 795 796
  , NP.Var 796 "IF_ELSE_FOOTER"
  , NP.Assign 797 "undefed" (NP.Num 0)
  , NP.Seq 797 798
  , NP.Assign 798 "stack_pointer" (NP.Num 0)
  , NP.Seq 798 799
  , NP.Var 799 "word"
  , NP.Seq 799 800
  , NP.Assign 800 "opcode" (NP.Num 0)
  , NP.Seq 800 801
  , NP.Assign 801 "oparg" (NP.Num 0)
  , NP.Seq 801 802
  , NP.Branch 802 (NP.Eq (NP.Num 0) (NP.Num 1)) 803 806
  , NP.Var 803 "word"
  , NP.Seq 803 804
  , NP.Assign 804 "opcode" (NP.Num 0)
  , NP.Seq 804 805
  , NP.Assign 805 "oparg" (NP.Num 0)
  , NP.Seq 805 806
  , NP.Seq 805 802
  , NP.Var 806 "LOOP_FOOTER"
  , NP.Seq 806 807
  , NP.Seq 806 35
  , NP.Branch 807 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 809 848
  , NP.Var 809 "NOP_809"
  , NP.Var 810 "__CLABEL_TARGET_BINARY_OP_SUBTRACT_INT"
  , NP.Seq 810 811
  , NP.Var 811 "this_instr"
  , NP.Seq 811 812
  , NP.Assign 812 "undefed" (NP.Num 0)
  , NP.Seq 812 813
  , NP.Assign 813 "next_instr" (NP.Num 0)
  , NP.Seq 813 814
  , NP.Var 814 "value"
  , NP.Seq 814 815
  , NP.Var 815 "left"
  , NP.Seq 815 816
  , NP.Var 816 "right"
  , NP.Seq 816 817
  , NP.Var 817 "res"
  , NP.Seq 817 818
  , NP.Assign 818 "value" (NP.Num 0)
  , NP.Seq 818 819
  , NP.Var 819 "value_o"
  , NP.Seq 819 820
  , NP.Branch 820 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 822 822
  , NP.Seq 821 42
  , NP.Seq 821 823
  , NP.Var 822 "NOP_822"
  , NP.Seq 822 823
  , NP.Var 823 "IF_ELSE_FOOTER"
  , NP.Assign 824 "left" (NP.Num 0)
  , NP.Seq 824 825
  , NP.Var 825 "left_o"
  , NP.Seq 825 826
  , NP.Branch 826 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 828 828
  , NP.Seq 827 42
  , NP.Seq 827 829
  , NP.Var 828 "NOP_828"
  , NP.Seq 828 829
  , NP.Var 829 "IF_ELSE_FOOTER"
  , NP.Assign 830 "right" (NP.Num 0)
  , NP.Seq 830 831
  , NP.Var 831 "left_o"
  , NP.Seq 831 832
  , NP.Var 832 "right_o"
  , NP.Seq 832 833
  , NP.Assign 833 "res" (NP.Num 0)
  , NP.Seq 833 834
  , NP.Branch 834 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 836 836
  , NP.Seq 835 42
  , NP.Seq 835 837
  , NP.Var 836 "NOP_836"
  , NP.Seq 836 837
  , NP.Var 837 "IF_ELSE_FOOTER"
  , NP.Assign 838 "undefed" (NP.Num 0)
  , NP.Seq 838 839
  , NP.Assign 839 "stack_pointer" (NP.Num 0)
  , NP.Seq 839 840
  , NP.Var 840 "word"
  , NP.Seq 840 841
  , NP.Assign 841 "opcode" (NP.Num 0)
  , NP.Seq 841 842
  , NP.Assign 842 "oparg" (NP.Num 0)
  , NP.Seq 842 843
  , NP.Branch 843 (NP.Eq (NP.Num 0) (NP.Num 1)) 844 847
  , NP.Var 844 "word"
  , NP.Seq 844 845
  , NP.Assign 845 "opcode" (NP.Num 0)
  , NP.Seq 845 846
  , NP.Assign 846 "oparg" (NP.Num 0)
  , NP.Seq 846 847
  , NP.Seq 846 843
  , NP.Var 847 "LOOP_FOOTER"
  , NP.Seq 847 848
  , NP.Seq 847 35
  , NP.Branch 848 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 850 931
  , NP.Var 850 "NOP_850"
  , NP.Var 851 "__CLABEL_TARGET_BINARY_SLICE"
  , NP.Seq 851 852
  , NP.Assign 852 "undefed" (NP.Num 0)
  , NP.Seq 852 853
  , NP.Assign 853 "next_instr" (NP.Num 0)
  , NP.Seq 853 854
  , NP.Var 854 "container"
  , NP.Seq 854 855
  , NP.Var 855 "start"
  , NP.Seq 855 856
  , NP.Var 856 "stop"
  , NP.Seq 856 857
  , NP.Var 857 "res"
  , NP.Seq 857 858
  , NP.Assign 858 "stop" (NP.Num 0)
  , NP.Seq 858 859
  , NP.Assign 859 "start" (NP.Num 0)
  , NP.Seq 859 860
  , NP.Assign 860 "container" (NP.Num 0)
  , NP.Seq 860 861
  , NP.Var 861 "slice"
  , NP.Seq 861 862
  , NP.Assign 862 "stack_pointer" (NP.Num 0)
  , NP.Seq 862 863
  , NP.Var 863 "res_o"
  , NP.Seq 863 864
  , NP.Branch 864 (NP.Eq (NP.Plus (NP.Id "slice") (NP.Num 0)) (NP.Num 1)) 866 867
  , NP.Assign 866 "res_o" (NP.Num 0)
  , NP.Seq 866 913
  , NP.Assign 867 "stack_pointer" (NP.Num 0)
  , NP.Seq 867 868
  , NP.Assign 868 "res_o" (NP.Num 0)
  , NP.Seq 868 869
  , NP.Var 869 "op"
  , NP.Seq 869 870
  , NP.Branch 870 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 0)) (NP.Num 1)) 872 887
  , NP.Var 872 "tracer"
  , NP.Seq 872 873
  , NP.Branch 873 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 875 876
  , NP.Var 875 "data"
  , NP.Seq 875 876
  , NP.Seq 875 877
  , NP.Var 876 "NOP_876"
  , NP.Seq 876 877
  , NP.Var 877 "IF_ELSE_FOOTER"
  , NP.Branch 878 (NP.Eq (NP.Num 0) (NP.Num 1)) 879 885
  , NP.Var 879 "tracer"
  , NP.Seq 879 880
  , NP.Branch 880 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 882 883
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
  , NP.Branch 889 (NP.Eq (NP.Num 0) (NP.Num 1)) 890 910
  , NP.Var 890 "op"
  , NP.Seq 890 891
  , NP.Branch 891 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 0)) (NP.Num 1)) 893 908
  , NP.Var 893 "tracer"
  , NP.Seq 893 894
  , NP.Branch 894 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 896 897
  , NP.Var 896 "data"
  , NP.Seq 896 897
  , NP.Seq 896 898
  , NP.Var 897 "NOP_897"
  , NP.Seq 897 898
  , NP.Var 898 "IF_ELSE_FOOTER"
  , NP.Branch 899 (NP.Eq (NP.Num 0) (NP.Num 1)) 900 906
  , NP.Var 900 "tracer"
  , NP.Seq 900 901
  , NP.Branch 901 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 903 904
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
  , NP.Assign 911 "stack_pointer" (NP.Num 0)
  , NP.Seq 911 912
  , NP.Assign 912 "stack_pointer" (NP.Num 0)
  , NP.Seq 912 913
  , NP.Var 913 "IF_ELSE_FOOTER"
  , NP.Assign 914 "stack_pointer" (NP.Num 0)
  , NP.Seq 914 915
  , NP.Assign 915 "stack_pointer" (NP.Num 0)
  , NP.Seq 915 916
  , NP.Branch 916 (NP.Eq (NP.Plus (NP.Id "res_o") (NP.Num 0)) (NP.Num 1)) 918 918
  , NP.Seq 917 1817
  , NP.Seq 917 919
  , NP.Var 918 "NOP_918"
  , NP.Seq 918 919
  , NP.Var 919 "IF_ELSE_FOOTER"
  , NP.Assign 920 "res" (NP.Num 0)
  , NP.Seq 920 921
  , NP.Assign 921 "undefed" (NP.Num 0)
  , NP.Seq 921 922
  , NP.Assign 922 "stack_pointer" (NP.Num 0)
  , NP.Seq 922 923
  , NP.Var 923 "word"
  , NP.Seq 923 924
  , NP.Assign 924 "opcode" (NP.Num 0)
  , NP.Seq 924 925
  , NP.Assign 925 "oparg" (NP.Num 0)
  , NP.Seq 925 926
  , NP.Branch 926 (NP.Eq (NP.Num 0) (NP.Num 1)) 927 930
  , NP.Var 927 "word"
  , NP.Seq 927 928
  , NP.Assign 928 "opcode" (NP.Num 0)
  , NP.Seq 928 929
  , NP.Assign 929 "oparg" (NP.Num 0)
  , NP.Seq 929 930
  , NP.Seq 929 926
  , NP.Var 930 "LOOP_FOOTER"
  , NP.Seq 930 931
  , NP.Seq 930 35
  , NP.Branch 931 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 933 980
  , NP.Var 933 "NOP_933"
  , NP.Var 934 "__CLABEL_TARGET_BUILD_INTERPOLATION"
  , NP.Seq 934 935
  , NP.Assign 935 "undefed" (NP.Num 0)
  , NP.Seq 935 936
  , NP.Assign 936 "next_instr" (NP.Num 0)
  , NP.Seq 936 937
  , NP.Var 937 "value"
  , NP.Seq 937 938
  , NP.Var 938 "str"
  , NP.Seq 938 939
  , NP.Var 939 "format"
  , NP.Seq 939 940
  , NP.Var 940 "interpolation"
  , NP.Seq 940 941
  , NP.Assign 941 "format" (NP.Num 0)
  , NP.Seq 941 942
  , NP.Assign 942 "str" (NP.Num 0)
  , NP.Seq 942 943
  , NP.Assign 943 "value" (NP.Num 0)
  , NP.Seq 943 944
  , NP.Var 944 "value_o"
  , NP.Seq 944 945
  , NP.Var 945 "str_o"
  , NP.Seq 945 946
  , NP.Var 946 "conversion"
  , NP.Seq 946 947
  , NP.Var 947 "format_o"
  , NP.Seq 947 948
  , NP.Branch 948 (NP.Eq (NP.Plus (NP.Id "oparg") (NP.Num 0)) (NP.Num 1)) 950 951
  , NP.Assign 950 "format_o" (NP.Num 0)
  , NP.Seq 950 952
  , NP.Assign 951 "format_o" (NP.Num 0)
  , NP.Seq 951 952
  , NP.Var 952 "IF_ELSE_FOOTER"
  , NP.Var 953 "interpolation_o"
  , NP.Seq 953 954
  , NP.Assign 954 "stack_pointer" (NP.Num 0)
  , NP.Seq 954 955
  , NP.Branch 955 (NP.Eq (NP.Plus (NP.Id "oparg") (NP.Num 0)) (NP.Num 1)) 957 959
  , NP.Assign 957 "stack_pointer" (NP.Num 0)
  , NP.Seq 957 958
  , NP.Assign 958 "stack_pointer" (NP.Num 0)
  , NP.Seq 958 959
  , NP.Seq 958 960
  , NP.Assign 959 "stack_pointer" (NP.Num 0)
  , NP.Seq 959 960
  , NP.Var 960 "IF_ELSE_FOOTER"
  , NP.Assign 961 "stack_pointer" (NP.Num 0)
  , NP.Seq 961 962
  , NP.Assign 962 "stack_pointer" (NP.Num 0)
  , NP.Seq 962 963
  , NP.Assign 963 "stack_pointer" (NP.Num 0)
  , NP.Seq 963 964
  , NP.Assign 964 "stack_pointer" (NP.Num 0)
  , NP.Seq 964 965
  , NP.Branch 965 (NP.Eq (NP.Plus (NP.Id "interpolation_o") (NP.Num 0)) (NP.Num 1)) 967 967
  , NP.Seq 966 1817
  , NP.Seq 966 968
  , NP.Var 967 "NOP_967"
  , NP.Seq 967 968
  , NP.Var 968 "IF_ELSE_FOOTER"
  , NP.Assign 969 "interpolation" (NP.Num 0)
  , NP.Seq 969 970
  , NP.Assign 970 "undefed" (NP.Num 0)
  , NP.Seq 970 971
  , NP.Assign 971 "stack_pointer" (NP.Num 0)
  , NP.Seq 971 972
  , NP.Var 972 "word"
  , NP.Seq 972 973
  , NP.Assign 973 "opcode" (NP.Num 0)
  , NP.Seq 973 974
  , NP.Assign 974 "oparg" (NP.Num 0)
  , NP.Seq 974 975
  , NP.Branch 975 (NP.Eq (NP.Num 0) (NP.Num 1)) 976 979
  , NP.Var 976 "word"
  , NP.Seq 976 977
  , NP.Assign 977 "opcode" (NP.Num 0)
  , NP.Seq 977 978
  , NP.Assign 978 "oparg" (NP.Num 0)
  , NP.Seq 978 979
  , NP.Seq 978 975
  , NP.Var 979 "LOOP_FOOTER"
  , NP.Seq 979 980
  , NP.Seq 979 35
  , NP.Branch 980 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 982 1006
  , NP.Var 982 "NOP_982"
  , NP.Var 983 "__CLABEL_TARGET_BUILD_LIST"
  , NP.Seq 983 984
  , NP.Assign 984 "undefed" (NP.Num 0)
  , NP.Seq 984 985
  , NP.Assign 985 "next_instr" (NP.Num 0)
  , NP.Seq 985 986
  , NP.Var 986 "values"
  , NP.Seq 986 987
  , NP.Var 987 "list"
  , NP.Seq 987 988
  , NP.Assign 988 "values" (NP.Num 0)
  , NP.Seq 988 989
  , NP.Var 989 "list_o"
  , NP.Seq 989 990
  , NP.Assign 990 "stack_pointer" (NP.Num 0)
  , NP.Seq 990 991
  , NP.Branch 991 (NP.Eq (NP.Plus (NP.Id "list_o") (NP.Num 0)) (NP.Num 1)) 993 993
  , NP.Seq 992 1817
  , NP.Seq 992 994
  , NP.Var 993 "NOP_993"
  , NP.Seq 993 994
  , NP.Var 994 "IF_ELSE_FOOTER"
  , NP.Assign 995 "list" (NP.Num 0)
  , NP.Seq 995 996
  , NP.Assign 996 "undefed" (NP.Num 0)
  , NP.Seq 996 997
  , NP.Assign 997 "stack_pointer" (NP.Num 0)
  , NP.Seq 997 998
  , NP.Var 998 "word"
  , NP.Seq 998 999
  , NP.Assign 999 "opcode" (NP.Num 0)
  , NP.Seq 999 1000
  , NP.Assign 1000 "oparg" (NP.Num 0)
  , NP.Seq 1000 1001
  , NP.Branch 1001 (NP.Eq (NP.Num 0) (NP.Num 1)) 1002 1005
  , NP.Var 1002 "word"
  , NP.Seq 1002 1003
  , NP.Assign 1003 "opcode" (NP.Num 0)
  , NP.Seq 1003 1004
  , NP.Assign 1004 "oparg" (NP.Num 0)
  , NP.Seq 1004 1005
  , NP.Seq 1004 1001
  , NP.Var 1005 "LOOP_FOOTER"
  , NP.Seq 1005 1006
  , NP.Seq 1005 35
  , NP.Branch 1006 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 1008 1054
  , NP.Var 1008 "NOP_1008"
  , NP.Var 1009 "__CLABEL_TARGET_BUILD_MAP"
  , NP.Seq 1009 1010
  , NP.Assign 1010 "undefed" (NP.Num 0)
  , NP.Seq 1010 1011
  , NP.Assign 1011 "next_instr" (NP.Num 0)
  , NP.Seq 1011 1012
  , NP.Var 1012 "values"
  , NP.Seq 1012 1013
  , NP.Var 1013 "map"
  , NP.Seq 1013 1014
  , NP.Assign 1014 "values" (NP.Num 0)
  , NP.Seq 1014 1015
  , NP.Var 1015 "values_o_temp"
  , NP.Seq 1015 1016
  , NP.Var 1016 "values_o"
  , NP.Seq 1016 1017
  , NP.Branch 1017 (NP.Eq (NP.Plus (NP.Id "values_o") (NP.Num 0)) (NP.Num 1)) 1019 1027
  , NP.Var 1019 "tmp"
  , NP.Seq 1019 1020
  , NP.Var 1020 "_i"
  , NP.Seq 1020 1021
  , NP.Branch 1021 (NP.Eq (NP.Plus (NP.Plus (NP.Id "_i") (NP.Num 0)) (NP.Num 0)) (NP.Num 1)) 1022 1024
  , NP.Assign 1022 "tmp" (NP.Num 0)
  , NP.Seq 1022 1023
  , NP.Assign 1023 "undefed" (NP.Num 0)
  , NP.Seq 1023 1024
  , NP.Seq 1023 1021
  , NP.Var 1024 "LOOP_FOOTER"
  , NP.Seq 1024 1025
  , NP.Assign 1025 "stack_pointer" (NP.Num 0)
  , NP.Seq 1025 1026
  , NP.Assign 1026 "stack_pointer" (NP.Num 0)
  , NP.Seq 1026 1027
  , NP.Seq 1026 1817
  , NP.Seq 1026 1028
  , NP.Var 1027 "NOP_1027"
  , NP.Seq 1027 1028
  , NP.Var 1028 "IF_ELSE_FOOTER"
  , NP.Var 1029 "map_o"
  , NP.Seq 1029 1030
  , NP.Assign 1030 "stack_pointer" (NP.Num 0)
  , NP.Seq 1030 1031
  , NP.Var 1031 "tmp"
  , NP.Seq 1031 1032
  , NP.Var 1032 "_i"
  , NP.Seq 1032 1033
  , NP.Branch 1033 (NP.Eq (NP.Plus (NP.Plus (NP.Id "_i") (NP.Num 0)) (NP.Num 0)) (NP.Num 1)) 1034 1036
  , NP.Assign 1034 "tmp" (NP.Num 0)
  , NP.Seq 1034 1035
  , NP.Assign 1035 "undefed" (NP.Num 0)
  , NP.Seq 1035 1036
  , NP.Seq 1035 1033
  , NP.Var 1036 "LOOP_FOOTER"
  , NP.Seq 1036 1037
  , NP.Assign 1037 "stack_pointer" (NP.Num 0)
  , NP.Seq 1037 1038
  , NP.Assign 1038 "stack_pointer" (NP.Num 0)
  , NP.Seq 1038 1039
  , NP.Branch 1039 (NP.Eq (NP.Plus (NP.Id "map_o") (NP.Num 0)) (NP.Num 1)) 1041 1041
  , NP.Seq 1040 1817
  , NP.Seq 1040 1042
  , NP.Var 1041 "NOP_1041"
  , NP.Seq 1041 1042
  , NP.Var 1042 "IF_ELSE_FOOTER"
  , NP.Assign 1043 "map" (NP.Num 0)
  , NP.Seq 1043 1044
  , NP.Assign 1044 "undefed" (NP.Num 0)
  , NP.Seq 1044 1045
  , NP.Assign 1045 "stack_pointer" (NP.Num 0)
  , NP.Seq 1045 1046
  , NP.Var 1046 "word"
  , NP.Seq 1046 1047
  , NP.Assign 1047 "opcode" (NP.Num 0)
  , NP.Seq 1047 1048
  , NP.Assign 1048 "oparg" (NP.Num 0)
  , NP.Seq 1048 1049
  , NP.Branch 1049 (NP.Eq (NP.Num 0) (NP.Num 1)) 1050 1053
  , NP.Var 1050 "word"
  , NP.Seq 1050 1051
  , NP.Assign 1051 "opcode" (NP.Num 0)
  , NP.Seq 1051 1052
  , NP.Assign 1052 "oparg" (NP.Num 0)
  , NP.Seq 1052 1053
  , NP.Seq 1052 1049
  , NP.Var 1053 "LOOP_FOOTER"
  , NP.Seq 1053 1054
  , NP.Seq 1053 35
  , NP.Branch 1054 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 1056 1148
  , NP.Var 1056 "NOP_1056"
  , NP.Var 1057 "__CLABEL_TARGET_BUILD_SET"
  , NP.Seq 1057 1058
  , NP.Assign 1058 "undefed" (NP.Num 0)
  , NP.Seq 1058 1059
  , NP.Assign 1059 "next_instr" (NP.Num 0)
  , NP.Seq 1059 1060
  , NP.Var 1060 "values"
  , NP.Seq 1060 1061
  , NP.Var 1061 "set"
  , NP.Seq 1061 1062
  , NP.Assign 1062 "values" (NP.Num 0)
  , NP.Seq 1062 1063
  , NP.Var 1063 "set_o"
  , NP.Seq 1063 1064
  , NP.Assign 1064 "stack_pointer" (NP.Num 0)
  , NP.Seq 1064 1065
  , NP.Branch 1065 (NP.Eq (NP.Plus (NP.Id "set_o") (NP.Num 0)) (NP.Num 1)) 1067 1075
  , NP.Var 1067 "tmp"
  , NP.Seq 1067 1068
  , NP.Var 1068 "_i"
  , NP.Seq 1068 1069
  , NP.Branch 1069 (NP.Eq (NP.Plus (NP.Plus (NP.Id "_i") (NP.Num 0)) (NP.Num 0)) (NP.Num 1)) 1070 1072
  , NP.Assign 1070 "tmp" (NP.Num 0)
  , NP.Seq 1070 1071
  , NP.Assign 1071 "undefed" (NP.Num 0)
  , NP.Seq 1071 1072
  , NP.Seq 1071 1069
  , NP.Var 1072 "LOOP_FOOTER"
  , NP.Seq 1072 1073
  , NP.Assign 1073 "stack_pointer" (NP.Num 0)
  , NP.Seq 1073 1074
  , NP.Assign 1074 "stack_pointer" (NP.Num 0)
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
  , NP.Branch 1079 (NP.Eq (NP.Plus (NP.Id "i") (NP.Id "oparg")) (NP.Num 1)) 1080 1088
  , NP.Var 1080 "value"
  , NP.Seq 1080 1081
  , NP.Assign 1081 "undefed" (NP.Num 0)
  , NP.Seq 1081 1082
  , NP.Branch 1082 (NP.Eq (NP.Plus (NP.Id "err") (NP.Num 0)) (NP.Num 1)) 1084 1086
  , NP.Assign 1084 "err" (NP.Num 0)
  , NP.Seq 1084 1085
  , NP.Assign 1085 "stack_pointer" (NP.Num 0)
  , NP.Seq 1085 1086
  , NP.Seq 1085 1087
  , NP.Assign 1086 "stack_pointer" (NP.Num 0)
  , NP.Seq 1086 1087
  , NP.Var 1087 "IF_ELSE_FOOTER"
  , NP.Seq 1087 1079
  , NP.Var 1088 "LOOP_FOOTER"
  , NP.Seq 1088 1089
  , NP.Branch 1089 (NP.Eq (NP.Id "err") (NP.Num 1)) 1091 1135
  , NP.Assign 1091 "stack_pointer" (NP.Num 0)
  , NP.Seq 1091 1092
  , NP.Var 1092 "op"
  , NP.Seq 1092 1093
  , NP.Branch 1093 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 0)) (NP.Num 1)) 1095 1110
  , NP.Var 1095 "tracer"
  , NP.Seq 1095 1096
  , NP.Branch 1096 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 1098 1099
  , NP.Var 1098 "data"
  , NP.Seq 1098 1099
  , NP.Seq 1098 1100
  , NP.Var 1099 "NOP_1099"
  , NP.Seq 1099 1100
  , NP.Var 1100 "IF_ELSE_FOOTER"
  , NP.Branch 1101 (NP.Eq (NP.Num 0) (NP.Num 1)) 1102 1108
  , NP.Var 1102 "tracer"
  , NP.Seq 1102 1103
  , NP.Branch 1103 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 1105 1106
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
  , NP.Branch 1112 (NP.Eq (NP.Num 0) (NP.Num 1)) 1113 1133
  , NP.Var 1113 "op"
  , NP.Seq 1113 1114
  , NP.Branch 1114 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 0)) (NP.Num 1)) 1116 1131
  , NP.Var 1116 "tracer"
  , NP.Seq 1116 1117
  , NP.Branch 1117 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 1119 1120
  , NP.Var 1119 "data"
  , NP.Seq 1119 1120
  , NP.Seq 1119 1121
  , NP.Var 1120 "NOP_1120"
  , NP.Seq 1120 1121
  , NP.Var 1121 "IF_ELSE_FOOTER"
  , NP.Branch 1122 (NP.Eq (NP.Num 0) (NP.Num 1)) 1123 1129
  , NP.Var 1123 "tracer"
  , NP.Seq 1123 1124
  , NP.Branch 1124 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 1126 1127
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
  , NP.Assign 1134 "stack_pointer" (NP.Num 0)
  , NP.Seq 1134 1135
  , NP.Seq 1134 1817
  , NP.Seq 1134 1136
  , NP.Var 1135 "NOP_1135"
  , NP.Seq 1135 1136
  , NP.Var 1136 "IF_ELSE_FOOTER"
  , NP.Assign 1137 "set" (NP.Num 0)
  , NP.Seq 1137 1138
  , NP.Assign 1138 "undefed" (NP.Num 0)
  , NP.Seq 1138 1139
  , NP.Assign 1139 "stack_pointer" (NP.Num 0)
  , NP.Seq 1139 1140
  , NP.Var 1140 "word"
  , NP.Seq 1140 1141
  , NP.Assign 1141 "opcode" (NP.Num 0)
  , NP.Seq 1141 1142
  , NP.Assign 1142 "oparg" (NP.Num 0)
  , NP.Seq 1142 1143
  , NP.Branch 1143 (NP.Eq (NP.Num 0) (NP.Num 1)) 1144 1147
  , NP.Var 1144 "word"
  , NP.Seq 1144 1145
  , NP.Assign 1145 "opcode" (NP.Num 0)
  , NP.Seq 1145 1146
  , NP.Assign 1146 "oparg" (NP.Num 0)
  , NP.Seq 1146 1147
  , NP.Seq 1146 1143
  , NP.Var 1147 "LOOP_FOOTER"
  , NP.Seq 1147 1148
  , NP.Seq 1147 35
  , NP.Branch 1148 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 1150 1184
  , NP.Var 1150 "NOP_1150"
  , NP.Var 1151 "__CLABEL_TARGET_BUILD_SLICE"
  , NP.Seq 1151 1152
  , NP.Assign 1152 "undefed" (NP.Num 0)
  , NP.Seq 1152 1153
  , NP.Assign 1153 "next_instr" (NP.Num 0)
  , NP.Seq 1153 1154
  , NP.Var 1154 "args"
  , NP.Seq 1154 1155
  , NP.Var 1155 "slice"
  , NP.Seq 1155 1156
  , NP.Assign 1156 "args" (NP.Num 0)
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
  , NP.Branch 1163 (NP.Eq (NP.Plus (NP.Plus (NP.Id "_i") (NP.Num 0)) (NP.Num 0)) (NP.Num 1)) 1164 1166
  , NP.Assign 1164 "tmp" (NP.Num 0)
  , NP.Seq 1164 1165
  , NP.Assign 1165 "undefed" (NP.Num 0)
  , NP.Seq 1165 1166
  , NP.Seq 1165 1163
  , NP.Var 1166 "LOOP_FOOTER"
  , NP.Seq 1166 1167
  , NP.Assign 1167 "stack_pointer" (NP.Num 0)
  , NP.Seq 1167 1168
  , NP.Assign 1168 "stack_pointer" (NP.Num 0)
  , NP.Seq 1168 1169
  , NP.Branch 1169 (NP.Eq (NP.Plus (NP.Id "slice_o") (NP.Num 0)) (NP.Num 1)) 1171 1171
  , NP.Seq 1170 1817
  , NP.Seq 1170 1172
  , NP.Var 1171 "NOP_1171"
  , NP.Seq 1171 1172
  , NP.Var 1172 "IF_ELSE_FOOTER"
  , NP.Assign 1173 "slice" (NP.Num 0)
  , NP.Seq 1173 1174
  , NP.Assign 1174 "undefed" (NP.Num 0)
  , NP.Seq 1174 1175
  , NP.Assign 1175 "stack_pointer" (NP.Num 0)
  , NP.Seq 1175 1176
  , NP.Var 1176 "word"
  , NP.Seq 1176 1177
  , NP.Assign 1177 "opcode" (NP.Num 0)
  , NP.Seq 1177 1178
  , NP.Assign 1178 "oparg" (NP.Num 0)
  , NP.Seq 1178 1179
  , NP.Branch 1179 (NP.Eq (NP.Num 0) (NP.Num 1)) 1180 1183
  , NP.Var 1180 "word"
  , NP.Seq 1180 1181
  , NP.Assign 1181 "opcode" (NP.Num 0)
  , NP.Seq 1181 1182
  , NP.Assign 1182 "oparg" (NP.Num 0)
  , NP.Seq 1182 1183
  , NP.Seq 1182 1179
  , NP.Var 1183 "LOOP_FOOTER"
  , NP.Seq 1183 1184
  , NP.Seq 1183 35
  , NP.Branch 1184 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 1186 1231
  , NP.Var 1186 "NOP_1186"
  , NP.Var 1187 "__CLABEL_TARGET_BUILD_STRING"
  , NP.Seq 1187 1188
  , NP.Assign 1188 "undefed" (NP.Num 0)
  , NP.Seq 1188 1189
  , NP.Assign 1189 "next_instr" (NP.Num 0)
  , NP.Seq 1189 1190
  , NP.Var 1190 "pieces"
  , NP.Seq 1190 1191
  , NP.Var 1191 "str"
  , NP.Seq 1191 1192
  , NP.Assign 1192 "pieces" (NP.Num 0)
  , NP.Seq 1192 1193
  , NP.Var 1193 "pieces_o_temp"
  , NP.Seq 1193 1194
  , NP.Var 1194 "pieces_o"
  , NP.Seq 1194 1195
  , NP.Branch 1195 (NP.Eq (NP.Plus (NP.Id "pieces_o") (NP.Num 0)) (NP.Num 1)) 1197 1205
  , NP.Var 1197 "tmp"
  , NP.Seq 1197 1198
  , NP.Var 1198 "_i"
  , NP.Seq 1198 1199
  , NP.Branch 1199 (NP.Eq (NP.Plus (NP.Plus (NP.Id "_i") (NP.Num 0)) (NP.Num 0)) (NP.Num 1)) 1200 1202
  , NP.Assign 1200 "tmp" (NP.Num 0)
  , NP.Seq 1200 1201
  , NP.Assign 1201 "undefed" (NP.Num 0)
  , NP.Seq 1201 1202
  , NP.Seq 1201 1199
  , NP.Var 1202 "LOOP_FOOTER"
  , NP.Seq 1202 1203
  , NP.Assign 1203 "stack_pointer" (NP.Num 0)
  , NP.Seq 1203 1204
  , NP.Assign 1204 "stack_pointer" (NP.Num 0)
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
  , NP.Branch 1210 (NP.Eq (NP.Plus (NP.Plus (NP.Id "_i") (NP.Num 0)) (NP.Num 0)) (NP.Num 1)) 1211 1213
  , NP.Assign 1211 "tmp" (NP.Num 0)
  , NP.Seq 1211 1212
  , NP.Assign 1212 "undefed" (NP.Num 0)
  , NP.Seq 1212 1213
  , NP.Seq 1212 1210
  , NP.Var 1213 "LOOP_FOOTER"
  , NP.Seq 1213 1214
  , NP.Assign 1214 "stack_pointer" (NP.Num 0)
  , NP.Seq 1214 1215
  , NP.Assign 1215 "stack_pointer" (NP.Num 0)
  , NP.Seq 1215 1216
  , NP.Branch 1216 (NP.Eq (NP.Plus (NP.Id "str_o") (NP.Num 0)) (NP.Num 1)) 1218 1218
  , NP.Seq 1217 1817
  , NP.Seq 1217 1219
  , NP.Var 1218 "NOP_1218"
  , NP.Seq 1218 1219
  , NP.Var 1219 "IF_ELSE_FOOTER"
  , NP.Assign 1220 "str" (NP.Num 0)
  , NP.Seq 1220 1221
  , NP.Assign 1221 "undefed" (NP.Num 0)
  , NP.Seq 1221 1222
  , NP.Assign 1222 "stack_pointer" (NP.Num 0)
  , NP.Seq 1222 1223
  , NP.Var 1223 "word"
  , NP.Seq 1223 1224
  , NP.Assign 1224 "opcode" (NP.Num 0)
  , NP.Seq 1224 1225
  , NP.Assign 1225 "oparg" (NP.Num 0)
  , NP.Seq 1225 1226
  , NP.Branch 1226 (NP.Eq (NP.Num 0) (NP.Num 1)) 1227 1230
  , NP.Var 1227 "word"
  , NP.Seq 1227 1228
  , NP.Assign 1228 "opcode" (NP.Num 0)
  , NP.Seq 1228 1229
  , NP.Assign 1229 "oparg" (NP.Num 0)
  , NP.Seq 1229 1230
  , NP.Seq 1229 1226
  , NP.Var 1230 "LOOP_FOOTER"
  , NP.Seq 1230 1231
  , NP.Seq 1230 35
  , NP.Branch 1231 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 1233 1265
  , NP.Var 1233 "NOP_1233"
  , NP.Var 1234 "__CLABEL_TARGET_BUILD_TEMPLATE"
  , NP.Seq 1234 1235
  , NP.Assign 1235 "undefed" (NP.Num 0)
  , NP.Seq 1235 1236
  , NP.Assign 1236 "next_instr" (NP.Num 0)
  , NP.Seq 1236 1237
  , NP.Var 1237 "strings"
  , NP.Seq 1237 1238
  , NP.Var 1238 "interpolations"
  , NP.Seq 1238 1239
  , NP.Var 1239 "template"
  , NP.Seq 1239 1240
  , NP.Assign 1240 "interpolations" (NP.Num 0)
  , NP.Seq 1240 1241
  , NP.Assign 1241 "strings" (NP.Num 0)
  , NP.Seq 1241 1242
  , NP.Var 1242 "strings_o"
  , NP.Seq 1242 1243
  , NP.Var 1243 "interpolations_o"
  , NP.Seq 1243 1244
  , NP.Var 1244 "template_o"
  , NP.Seq 1244 1245
  , NP.Assign 1245 "stack_pointer" (NP.Num 0)
  , NP.Seq 1245 1246
  , NP.Assign 1246 "stack_pointer" (NP.Num 0)
  , NP.Seq 1246 1247
  , NP.Assign 1247 "stack_pointer" (NP.Num 0)
  , NP.Seq 1247 1248
  , NP.Assign 1248 "stack_pointer" (NP.Num 0)
  , NP.Seq 1248 1249
  , NP.Assign 1249 "stack_pointer" (NP.Num 0)
  , NP.Seq 1249 1250
  , NP.Branch 1250 (NP.Eq (NP.Plus (NP.Id "template_o") (NP.Num 0)) (NP.Num 1)) 1252 1252
  , NP.Seq 1251 1817
  , NP.Seq 1251 1253
  , NP.Var 1252 "NOP_1252"
  , NP.Seq 1252 1253
  , NP.Var 1253 "IF_ELSE_FOOTER"
  , NP.Assign 1254 "template" (NP.Num 0)
  , NP.Seq 1254 1255
  , NP.Assign 1255 "undefed" (NP.Num 0)
  , NP.Seq 1255 1256
  , NP.Assign 1256 "stack_pointer" (NP.Num 0)
  , NP.Seq 1256 1257
  , NP.Var 1257 "word"
  , NP.Seq 1257 1258
  , NP.Assign 1258 "opcode" (NP.Num 0)
  , NP.Seq 1258 1259
  , NP.Assign 1259 "oparg" (NP.Num 0)
  , NP.Seq 1259 1260
  , NP.Branch 1260 (NP.Eq (NP.Num 0) (NP.Num 1)) 1261 1264
  , NP.Var 1261 "word"
  , NP.Seq 1261 1262
  , NP.Assign 1262 "opcode" (NP.Num 0)
  , NP.Seq 1262 1263
  , NP.Assign 1263 "oparg" (NP.Num 0)
  , NP.Seq 1263 1264
  , NP.Seq 1263 1260
  , NP.Var 1264 "LOOP_FOOTER"
  , NP.Seq 1264 1265
  , NP.Seq 1264 35
  , NP.Branch 1265 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 1267 1290
  , NP.Var 1267 "NOP_1267"
  , NP.Var 1268 "__CLABEL_TARGET_BUILD_TUPLE"
  , NP.Seq 1268 1269
  , NP.Assign 1269 "undefed" (NP.Num 0)
  , NP.Seq 1269 1270
  , NP.Assign 1270 "next_instr" (NP.Num 0)
  , NP.Seq 1270 1271
  , NP.Var 1271 "values"
  , NP.Seq 1271 1272
  , NP.Var 1272 "tup"
  , NP.Seq 1272 1273
  , NP.Assign 1273 "values" (NP.Num 0)
  , NP.Seq 1273 1274
  , NP.Var 1274 "tup_o"
  , NP.Seq 1274 1275
  , NP.Branch 1275 (NP.Eq (NP.Plus (NP.Id "tup_o") (NP.Num 0)) (NP.Num 1)) 1277 1277
  , NP.Seq 1276 1817
  , NP.Seq 1276 1278
  , NP.Var 1277 "NOP_1277"
  , NP.Seq 1277 1278
  , NP.Var 1278 "IF_ELSE_FOOTER"
  , NP.Assign 1279 "tup" (NP.Num 0)
  , NP.Seq 1279 1280
  , NP.Assign 1280 "undefed" (NP.Num 0)
  , NP.Seq 1280 1281
  , NP.Assign 1281 "stack_pointer" (NP.Num 0)
  , NP.Seq 1281 1282
  , NP.Var 1282 "word"
  , NP.Seq 1282 1283
  , NP.Assign 1283 "opcode" (NP.Num 0)
  , NP.Seq 1283 1284
  , NP.Assign 1284 "oparg" (NP.Num 0)
  , NP.Seq 1284 1285
  , NP.Branch 1285 (NP.Eq (NP.Num 0) (NP.Num 1)) 1286 1289
  , NP.Var 1286 "word"
  , NP.Seq 1286 1287
  , NP.Assign 1287 "opcode" (NP.Num 0)
  , NP.Seq 1287 1288
  , NP.Assign 1288 "oparg" (NP.Num 0)
  , NP.Seq 1288 1289
  , NP.Seq 1288 1285
  , NP.Var 1289 "LOOP_FOOTER"
  , NP.Seq 1289 1290
  , NP.Seq 1289 35
  , NP.Branch 1290 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 1292 1304
  , NP.Var 1292 "NOP_1292"
  , NP.Var 1293 "__CLABEL_TARGET_CACHE"
  , NP.Seq 1293 1294
  , NP.Assign 1294 "undefed" (NP.Num 0)
  , NP.Seq 1294 1295
  , NP.Assign 1295 "next_instr" (NP.Num 0)
  , NP.Seq 1295 1296
  , NP.Var 1296 "word"
  , NP.Seq 1296 1297
  , NP.Assign 1297 "opcode" (NP.Num 0)
  , NP.Seq 1297 1298
  , NP.Assign 1298 "oparg" (NP.Num 0)
  , NP.Seq 1298 1299
  , NP.Branch 1299 (NP.Eq (NP.Num 0) (NP.Num 1)) 1300 1303
  , NP.Var 1300 "word"
  , NP.Seq 1300 1301
  , NP.Assign 1301 "opcode" (NP.Num 0)
  , NP.Seq 1301 1302
  , NP.Assign 1302 "oparg" (NP.Num 0)
  , NP.Seq 1302 1303
  , NP.Seq 1302 1299
  , NP.Var 1303 "LOOP_FOOTER"
  , NP.Seq 1303 1304
  , NP.Seq 1303 35
  , NP.Branch 1304 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 1306 1551
  , NP.Var 1306 "NOP_1306"
  , NP.Var 1307 "__CLABEL_TARGET_CALL"
  , NP.Seq 1307 1308
  , NP.Assign 1308 "undefed" (NP.Num 0)
  , NP.Seq 1308 1309
  , NP.Assign 1309 "next_instr" (NP.Num 0)
  , NP.Seq 1309 1310
  , NP.Var 1310 "__CLABEL_PREDICTED_CALL"
  , NP.Seq 1310 1311
  , NP.Var 1311 "NOP_1311"
  , NP.Var 1312 "this_instr"
  , NP.Seq 1312 1313
  , NP.Assign 1313 "opcode" (NP.Num 0)
  , NP.Seq 1313 1314
  , NP.Var 1314 "callable"
  , NP.Seq 1314 1315
  , NP.Var 1315 "self_or_null"
  , NP.Seq 1315 1316
  , NP.Var 1316 "args"
  , NP.Seq 1316 1317
  , NP.Var 1317 "res"
  , NP.Seq 1317 1318
  , NP.Assign 1318 "self_or_null" (NP.Num 0)
  , NP.Seq 1318 1319
  , NP.Assign 1319 "callable" (NP.Num 0)
  , NP.Seq 1319 1320
  , NP.Var 1320 "counter"
  , NP.Seq 1320 1321
  , NP.Branch 1321 (NP.Eq (NP.Num 0) (NP.Num 1)) 1323 1326
  , NP.Assign 1323 "next_instr" (NP.Num 0)
  , NP.Seq 1323 1324
  , NP.Assign 1324 "stack_pointer" (NP.Num 0)
  , NP.Seq 1324 1325
  , NP.Assign 1325 "opcode" (NP.Num 0)
  , NP.Seq 1325 1326
  , NP.Seq 1325 35
  , NP.Seq 1325 1327
  , NP.Var 1326 "NOP_1326"
  , NP.Seq 1326 1327
  , NP.Var 1327 "IF_ELSE_FOOTER"
  , NP.Assign 1328 "undefed" (NP.Num 0)
  , NP.Seq 1328 1329
  , NP.Branch 1329 (NP.Eq (NP.Num 0) (NP.Num 1)) 1330 1331
  , NP.Assign 1330 "undefed" (NP.Num 0)
  , NP.Seq 1330 1329
  , NP.Var 1331 "LOOP_FOOTER"
  , NP.Seq 1331 1332
  , NP.Branch 1332 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Plus (NP.Id "PyMethod_Type") (NP.Num 0))) (NP.Plus (NP.Num 0) (NP.Num 0))) (NP.Num 1)) 1334 1343
  , NP.Var 1334 "callable_o"
  , NP.Seq 1334 1335
  , NP.Var 1335 "self"
  , NP.Seq 1335 1336
  , NP.Assign 1336 "self_or_null" (NP.Num 0)
  , NP.Seq 1336 1337
  , NP.Var 1337 "method"
  , NP.Seq 1337 1338
  , NP.Var 1338 "temp"
  , NP.Seq 1338 1339
  , NP.Assign 1339 "callable" (NP.Num 0)
  , NP.Seq 1339 1340
  , NP.Assign 1340 "undefed" (NP.Num 0)
  , NP.Seq 1340 1341
  , NP.Assign 1341 "undefed" (NP.Num 0)
  , NP.Seq 1341 1342
  , NP.Assign 1342 "stack_pointer" (NP.Num 0)
  , NP.Seq 1342 1343
  , NP.Seq 1342 1344
  , NP.Var 1343 "NOP_1343"
  , NP.Seq 1343 1344
  , NP.Var 1344 "IF_ELSE_FOOTER"
  , NP.Assign 1345 "args" (NP.Num 0)
  , NP.Seq 1345 1346
  , NP.Var 1346 "callable_o"
  , NP.Seq 1346 1347
  , NP.Var 1347 "total_args"
  , NP.Seq 1347 1348
  , NP.Var 1348 "arguments"
  , NP.Seq 1348 1349
  , NP.Branch 1349 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 0)) (NP.Num 1)) 1351 1352
  , NP.Assign 1351 "total_args" (NP.Num 0)
  , NP.Seq 1351 1352
  , NP.Seq 1351 1353
  , NP.Var 1352 "NOP_1352"
  , NP.Seq 1352 1353
  , NP.Var 1353 "IF_ELSE_FOOTER"
  , NP.Branch 1354 (NP.Eq (NP.Plus (NP.Plus (NP.Plus (NP.Num 0) (NP.Plus (NP.Id "PyFunction_Type") (NP.Num 0))) (NP.Plus (NP.Num 0) (NP.Num 0))) (NP.Plus (NP.Num 0) (NP.Id "_PyFunction_Vectorcall"))) (NP.Num 1)) 1356 1372
  , NP.Var 1356 "code_flags"
  , NP.Seq 1356 1357
  , NP.Var 1357 "locals"
  , NP.Seq 1357 1358
  , NP.Assign 1358 "undefed" (NP.Num 0)
  , NP.Seq 1358 1359
  , NP.Assign 1359 "undefed" (NP.Num 0)
  , NP.Seq 1359 1360
  , NP.Var 1360 "new_frame"
  , NP.Seq 1360 1361
  , NP.Assign 1361 "stack_pointer" (NP.Num 0)
  , NP.Seq 1361 1362
  , NP.Assign 1362 "stack_pointer" (NP.Num 0)
  , NP.Seq 1362 1363
  , NP.Branch 1363 (NP.Eq (NP.Plus (NP.Id "new_frame") (NP.Num 0)) (NP.Num 1)) 1365 1365
  , NP.Seq 1364 1817
  , NP.Seq 1364 1366
  , NP.Var 1365 "NOP_1365"
  , NP.Seq 1365 1366
  , NP.Var 1366 "IF_ELSE_FOOTER"
  , NP.Assign 1367 "undefed" (NP.Num 0)
  , NP.Seq 1367 1368
  , NP.Assign 1368 "frame" (NP.Num 0)
  , NP.Seq 1368 1369
  , NP.Seq 1368 1886
  , NP.Branch 1369 (NP.Eq (NP.Num 0) (NP.Num 1)) 1370 1371
  , NP.Assign 1370 "frame" (NP.Num 0)
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
  , NP.Branch 1376 (NP.Eq (NP.Plus (NP.Id "args_o") (NP.Num 0)) (NP.Num 1)) 1378 1394
  , NP.Var 1378 "tmp"
  , NP.Seq 1378 1379
  , NP.Var 1379 "_i"
  , NP.Seq 1379 1380
  , NP.Branch 1380 (NP.Eq (NP.Plus (NP.Plus (NP.Id "_i") (NP.Num 0)) (NP.Num 0)) (NP.Num 1)) 1381 1385
  , NP.Assign 1381 "tmp" (NP.Num 0)
  , NP.Seq 1381 1382
  , NP.Assign 1382 "undefed" (NP.Num 0)
  , NP.Seq 1382 1383
  , NP.Assign 1383 "undefed" (NP.Num 0)
  , NP.Seq 1383 1384
  , NP.Assign 1384 "undefed" (NP.Num 0)
  , NP.Seq 1384 1385
  , NP.Seq 1384 1380
  , NP.Var 1385 "LOOP_FOOTER"
  , NP.Seq 1385 1386
  , NP.Assign 1386 "tmp" (NP.Num 0)
  , NP.Seq 1386 1387
  , NP.Assign 1387 "self_or_null" (NP.Num 0)
  , NP.Seq 1387 1388
  , NP.Assign 1388 "undefed" (NP.Num 0)
  , NP.Seq 1388 1389
  , NP.Assign 1389 "tmp" (NP.Num 0)
  , NP.Seq 1389 1390
  , NP.Assign 1390 "callable" (NP.Num 0)
  , NP.Seq 1390 1391
  , NP.Assign 1391 "undefed" (NP.Num 0)
  , NP.Seq 1391 1392
  , NP.Assign 1392 "stack_pointer" (NP.Num 0)
  , NP.Seq 1392 1393
  , NP.Assign 1393 "stack_pointer" (NP.Num 0)
  , NP.Seq 1393 1394
  , NP.Seq 1393 1817
  , NP.Seq 1393 1395
  , NP.Var 1394 "NOP_1394"
  , NP.Seq 1394 1395
  , NP.Var 1395 "IF_ELSE_FOOTER"
  , NP.Assign 1396 "undefed" (NP.Num 0)
  , NP.Seq 1396 1397
  , NP.Assign 1397 "undefed" (NP.Num 0)
  , NP.Seq 1397 1398
  , NP.Var 1398 "res_o"
  , NP.Seq 1398 1399
  , NP.Assign 1399 "stack_pointer" (NP.Num 0)
  , NP.Seq 1399 1400
  , NP.Branch 1400 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 1402 1514
  , NP.Var 1402 "arg"
  , NP.Seq 1402 1403
  , NP.Branch 1403 (NP.Eq (NP.Plus (NP.Id "res_o") (NP.Num 0)) (NP.Num 1)) 1405 1406
  , NP.Assign 1405 "stack_pointer" (NP.Num 0)
  , NP.Seq 1405 1513
  , NP.Var 1406 "err"
  , NP.Seq 1406 1407
  , NP.Assign 1407 "stack_pointer" (NP.Num 0)
  , NP.Seq 1407 1408
  , NP.Branch 1408 (NP.Eq (NP.Plus (NP.Id "err") (NP.Num 0)) (NP.Num 1)) 1410 1511
  , NP.Assign 1410 "undefed" (NP.Num 0)
  , NP.Seq 1410 1411
  , NP.Assign 1411 "_tmp_old_op" (NP.Num 0)
  , NP.Seq 1411 1412
  , NP.Branch 1412 (NP.Eq (NP.Plus (NP.Id "_tmp_old_op") (NP.Num 0)) (NP.Num 1)) 1414 1457
  , NP.Assign 1414 "undefed" (NP.Num 0)
  , NP.Seq 1414 1415
  , NP.Var 1415 "op"
  , NP.Seq 1415 1416
  , NP.Branch 1416 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 0)) (NP.Num 1)) 1418 1433
  , NP.Var 1418 "tracer"
  , NP.Seq 1418 1419
  , NP.Branch 1419 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 1421 1422
  , NP.Var 1421 "data"
  , NP.Seq 1421 1422
  , NP.Seq 1421 1423
  , NP.Var 1422 "NOP_1422"
  , NP.Seq 1422 1423
  , NP.Var 1423 "IF_ELSE_FOOTER"
  , NP.Branch 1424 (NP.Eq (NP.Num 0) (NP.Num 1)) 1425 1431
  , NP.Var 1425 "tracer"
  , NP.Seq 1425 1426
  , NP.Branch 1426 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 1428 1429
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
  , NP.Branch 1435 (NP.Eq (NP.Num 0) (NP.Num 1)) 1436 1456
  , NP.Var 1436 "op"
  , NP.Seq 1436 1437
  , NP.Branch 1437 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 0)) (NP.Num 1)) 1439 1454
  , NP.Var 1439 "tracer"
  , NP.Seq 1439 1440
  , NP.Branch 1440 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 1442 1443
  , NP.Var 1442 "data"
  , NP.Seq 1442 1443
  , NP.Seq 1442 1444
  , NP.Var 1443 "NOP_1443"
  , NP.Seq 1443 1444
  , NP.Var 1444 "IF_ELSE_FOOTER"
  , NP.Branch 1445 (NP.Eq (NP.Num 0) (NP.Num 1)) 1446 1452
  , NP.Var 1446 "tracer"
  , NP.Seq 1446 1447
  , NP.Branch 1447 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 1449 1450
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
  , NP.Branch 1459 (NP.Eq (NP.Num 0) (NP.Num 1)) 1460 1509
  , NP.Assign 1460 "undefed" (NP.Num 0)
  , NP.Seq 1460 1461
  , NP.Assign 1461 "_tmp_old_op" (NP.Num 0)
  , NP.Seq 1461 1462
  , NP.Branch 1462 (NP.Eq (NP.Plus (NP.Id "_tmp_old_op") (NP.Num 0)) (NP.Num 1)) 1464 1507
  , NP.Assign 1464 "undefed" (NP.Num 0)
  , NP.Seq 1464 1465
  , NP.Var 1465 "op"
  , NP.Seq 1465 1466
  , NP.Branch 1466 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 0)) (NP.Num 1)) 1468 1483
  , NP.Var 1468 "tracer"
  , NP.Seq 1468 1469
  , NP.Branch 1469 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 1471 1472
  , NP.Var 1471 "data"
  , NP.Seq 1471 1472
  , NP.Seq 1471 1473
  , NP.Var 1472 "NOP_1472"
  , NP.Seq 1472 1473
  , NP.Var 1473 "IF_ELSE_FOOTER"
  , NP.Branch 1474 (NP.Eq (NP.Num 0) (NP.Num 1)) 1475 1481
  , NP.Var 1475 "tracer"
  , NP.Seq 1475 1476
  , NP.Branch 1476 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 1478 1479
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
  , NP.Branch 1485 (NP.Eq (NP.Num 0) (NP.Num 1)) 1486 1506
  , NP.Var 1486 "op"
  , NP.Seq 1486 1487
  , NP.Branch 1487 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 0)) (NP.Num 1)) 1489 1504
  , NP.Var 1489 "tracer"
  , NP.Seq 1489 1490
  , NP.Branch 1490 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 1492 1493
  , NP.Var 1492 "data"
  , NP.Seq 1492 1493
  , NP.Seq 1492 1494
  , NP.Var 1493 "NOP_1493"
  , NP.Seq 1493 1494
  , NP.Var 1494 "IF_ELSE_FOOTER"
  , NP.Branch 1495 (NP.Eq (NP.Num 0) (NP.Num 1)) 1496 1502
  , NP.Var 1496 "tracer"
  , NP.Seq 1496 1497
  , NP.Branch 1497 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 1499 1500
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
  , NP.Assign 1510 "stack_pointer" (NP.Num 0)
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
  , NP.Branch 1518 (NP.Eq (NP.Plus (NP.Plus (NP.Id "_i") (NP.Num 0)) (NP.Num 0)) (NP.Num 1)) 1519 1521
  , NP.Assign 1519 "tmp" (NP.Num 0)
  , NP.Seq 1519 1520
  , NP.Assign 1520 "undefed" (NP.Num 0)
  , NP.Seq 1520 1521
  , NP.Seq 1520 1518
  , NP.Var 1521 "LOOP_FOOTER"
  , NP.Seq 1521 1522
  , NP.Assign 1522 "tmp" (NP.Num 0)
  , NP.Seq 1522 1523
  , NP.Assign 1523 "self_or_null" (NP.Num 0)
  , NP.Seq 1523 1524
  , NP.Assign 1524 "undefed" (NP.Num 0)
  , NP.Seq 1524 1525
  , NP.Assign 1525 "tmp" (NP.Num 0)
  , NP.Seq 1525 1526
  , NP.Assign 1526 "callable" (NP.Num 0)
  , NP.Seq 1526 1527
  , NP.Assign 1527 "undefed" (NP.Num 0)
  , NP.Seq 1527 1528
  , NP.Assign 1528 "stack_pointer" (NP.Num 0)
  , NP.Seq 1528 1529
  , NP.Assign 1529 "stack_pointer" (NP.Num 0)
  , NP.Seq 1529 1530
  , NP.Branch 1530 (NP.Eq (NP.Plus (NP.Id "res_o") (NP.Num 0)) (NP.Num 1)) 1532 1532
  , NP.Seq 1531 1817
  , NP.Seq 1531 1533
  , NP.Var 1532 "NOP_1532"
  , NP.Seq 1532 1533
  , NP.Var 1533 "IF_ELSE_FOOTER"
  , NP.Assign 1534 "res" (NP.Num 0)
  , NP.Seq 1534 1535
  , NP.Assign 1535 "undefed" (NP.Num 0)
  , NP.Seq 1535 1536
  , NP.Assign 1536 "stack_pointer" (NP.Num 0)
  , NP.Seq 1536 1537
  , NP.Var 1537 "err"
  , NP.Seq 1537 1538
  , NP.Assign 1538 "stack_pointer" (NP.Num 0)
  , NP.Seq 1538 1539
  , NP.Branch 1539 (NP.Eq (NP.Plus (NP.Id "err") (NP.Num 0)) (NP.Num 1)) 1541 1541
  , NP.Seq 1540 1817
  , NP.Seq 1540 1542
  , NP.Var 1541 "NOP_1541"
  , NP.Seq 1541 1542
  , NP.Var 1542 "IF_ELSE_FOOTER"
  , NP.Var 1543 "word"
  , NP.Seq 1543 1544
  , NP.Assign 1544 "opcode" (NP.Num 0)
  , NP.Seq 1544 1545
  , NP.Assign 1545 "oparg" (NP.Num 0)
  , NP.Seq 1545 1546
  , NP.Branch 1546 (NP.Eq (NP.Num 0) (NP.Num 1)) 1547 1550
  , NP.Var 1547 "word"
  , NP.Seq 1547 1548
  , NP.Assign 1548 "opcode" (NP.Num 0)
  , NP.Seq 1548 1549
  , NP.Assign 1549 "oparg" (NP.Num 0)
  , NP.Seq 1549 1550
  , NP.Seq 1549 1546
  , NP.Var 1550 "LOOP_FOOTER"
  , NP.Seq 1550 1551
  , NP.Seq 1550 35
  , NP.Branch 1551 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 1553 1637
  , NP.Var 1553 "NOP_1553"
  , NP.Var 1554 "__CLABEL_TARGET_CALL_ALLOC_AND_ENTER_INIT"
  , NP.Seq 1554 1555
  , NP.Var 1555 "this_instr"
  , NP.Seq 1555 1556
  , NP.Assign 1556 "undefed" (NP.Num 0)
  , NP.Seq 1556 1557
  , NP.Assign 1557 "next_instr" (NP.Num 0)
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
  , NP.Branch 1565 (NP.Eq (NP.Num 0) (NP.Num 1)) 1567 1567
  , NP.Seq 1566 1310
  , NP.Seq 1566 1568
  , NP.Var 1567 "NOP_1567"
  , NP.Seq 1567 1568
  , NP.Var 1568 "IF_ELSE_FOOTER"
  , NP.Assign 1569 "self_or_null" (NP.Num 0)
  , NP.Seq 1569 1570
  , NP.Assign 1570 "callable" (NP.Num 0)
  , NP.Seq 1570 1571
  , NP.Var 1571 "type_version"
  , NP.Seq 1571 1572
  , NP.Var 1572 "callable_o"
  , NP.Seq 1572 1573
  , NP.Branch 1573 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 0)) (NP.Num 1)) 1575 1575
  , NP.Seq 1574 1310
  , NP.Seq 1574 1576
  , NP.Var 1575 "NOP_1575"
  , NP.Seq 1575 1576
  , NP.Var 1576 "IF_ELSE_FOOTER"
  , NP.Branch 1577 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 1579 1579
  , NP.Seq 1578 1310
  , NP.Seq 1578 1580
  , NP.Var 1579 "NOP_1579"
  , NP.Seq 1579 1580
  , NP.Var 1580 "IF_ELSE_FOOTER"
  , NP.Var 1581 "tp"
  , NP.Seq 1581 1582
  , NP.Branch 1582 (NP.Eq (NP.Plus (NP.Num 0) (NP.Id "type_version")) (NP.Num 1)) 1584 1584
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
  , NP.Branch 1589 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 1591 1591
  , NP.Seq 1590 1310
  , NP.Seq 1590 1592
  , NP.Var 1591 "NOP_1591"
  , NP.Seq 1591 1592
  , NP.Var 1592 "IF_ELSE_FOOTER"
  , NP.Var 1593 "self_o"
  , NP.Seq 1593 1594
  , NP.Assign 1594 "stack_pointer" (NP.Num 0)
  , NP.Seq 1594 1595
  , NP.Branch 1595 (NP.Eq (NP.Plus (NP.Id "self_o") (NP.Num 0)) (NP.Num 1)) 1597 1597
  , NP.Seq 1596 1817
  , NP.Seq 1596 1598
  , NP.Var 1597 "NOP_1597"
  , NP.Seq 1597 1598
  , NP.Var 1598 "IF_ELSE_FOOTER"
  , NP.Assign 1599 "self_or_null" (NP.Num 0)
  , NP.Seq 1599 1600
  , NP.Var 1600 "temp"
  , NP.Seq 1600 1601
  , NP.Assign 1601 "callable" (NP.Num 0)
  , NP.Seq 1601 1602
  , NP.Assign 1602 "undefed" (NP.Num 0)
  , NP.Seq 1602 1603
  , NP.Assign 1603 "undefed" (NP.Num 0)
  , NP.Seq 1603 1604
  , NP.Assign 1604 "stack_pointer" (NP.Num 0)
  , NP.Seq 1604 1605
  , NP.Assign 1605 "args" (NP.Num 0)
  , NP.Seq 1605 1606
  , NP.Assign 1606 "self" (NP.Num 0)
  , NP.Seq 1606 1607
  , NP.Assign 1607 "init" (NP.Num 0)
  , NP.Seq 1607 1608
  , NP.Var 1608 "shim"
  , NP.Seq 1608 1609
  , NP.Assign 1609 "stack_pointer" (NP.Num 0)
  , NP.Seq 1609 1610
  , NP.Assign 1610 "undefed" (NP.Num 0)
  , NP.Seq 1610 1611
  , NP.Var 1611 "temp"
  , NP.Seq 1611 1612
  , NP.Assign 1612 "stack_pointer" (NP.Num 0)
  , NP.Seq 1612 1613
  , NP.Assign 1613 "stack_pointer" (NP.Num 0)
  , NP.Seq 1613 1614
  , NP.Branch 1614 (NP.Eq (NP.Plus (NP.Id "temp") (NP.Num 0)) (NP.Num 1)) 1616 1617
  , NP.Assign 1616 "stack_pointer" (NP.Num 0)
  , NP.Seq 1616 1617
  , NP.Seq 1616 1817
  , NP.Seq 1616 1618
  , NP.Var 1617 "NOP_1617"
  , NP.Seq 1617 1618
  , NP.Var 1618 "IF_ELSE_FOOTER"
  , NP.Assign 1619 "undefed" (NP.Num 0)
  , NP.Seq 1619 1620
  , NP.Assign 1620 "init_frame" (NP.Num 0)
  , NP.Seq 1620 1621
  , NP.Assign 1621 "new_frame" (NP.Num 0)
  , NP.Seq 1621 1622
  , NP.Var 1622 "temp"
  , NP.Seq 1622 1623
  , NP.Assign 1623 "frame" (NP.Num 0)
  , NP.Seq 1623 1624
  , NP.Assign 1624 "stack_pointer" (NP.Num 0)
  , NP.Seq 1624 1625
  , NP.Assign 1625 "next_instr" (NP.Num 0)
  , NP.Seq 1625 1626
  , NP.Branch 1626 (NP.Eq (NP.Num 0) (NP.Num 1)) 1627 1628
  , NP.Assign 1627 "next_instr" (NP.Num 0)
  , NP.Seq 1627 1626
  , NP.Var 1628 "LOOP_FOOTER"
  , NP.Seq 1628 1629
  , NP.Var 1629 "word"
  , NP.Seq 1629 1630
  , NP.Assign 1630 "opcode" (NP.Num 0)
  , NP.Seq 1630 1631
  , NP.Assign 1631 "oparg" (NP.Num 0)
  , NP.Seq 1631 1632
  , NP.Branch 1632 (NP.Eq (NP.Num 0) (NP.Num 1)) 1633 1636
  , NP.Var 1633 "word"
  , NP.Seq 1633 1634
  , NP.Assign 1634 "opcode" (NP.Num 0)
  , NP.Seq 1634 1635
  , NP.Assign 1635 "oparg" (NP.Num 0)
  , NP.Seq 1635 1636
  , NP.Seq 1635 1632
  , NP.Var 1636 "LOOP_FOOTER"
  , NP.Seq 1636 1637
  , NP.Seq 1636 35
  , NP.Branch 1637 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 1639 1727
  , NP.Var 1639 "NOP_1639"
  , NP.Var 1640 "__CLABEL_TARGET_CALL_BOUND_METHOD_EXACT_ARGS"
  , NP.Seq 1640 1641
  , NP.Var 1641 "this_instr"
  , NP.Seq 1641 1642
  , NP.Assign 1642 "undefed" (NP.Num 0)
  , NP.Seq 1642 1643
  , NP.Assign 1643 "next_instr" (NP.Num 0)
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
  , NP.Branch 1649 (NP.Eq (NP.Num 0) (NP.Num 1)) 1651 1651
  , NP.Seq 1650 1310
  , NP.Seq 1650 1652
  , NP.Var 1651 "NOP_1651"
  , NP.Seq 1651 1652
  , NP.Var 1652 "IF_ELSE_FOOTER"
  , NP.Assign 1653 "null" (NP.Num 0)
  , NP.Seq 1653 1654
  , NP.Assign 1654 "callable" (NP.Num 0)
  , NP.Seq 1654 1655
  , NP.Branch 1655 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 0)) (NP.Num 1)) 1657 1657
  , NP.Seq 1656 1310
  , NP.Seq 1656 1658
  , NP.Var 1657 "NOP_1657"
  , NP.Seq 1657 1658
  , NP.Var 1658 "IF_ELSE_FOOTER"
  , NP.Branch 1659 (NP.Eq (NP.Plus (NP.Num 0) (NP.Plus (NP.Id "PyMethod_Type") (NP.Num 0))) (NP.Num 1)) 1661 1661
  , NP.Seq 1660 1310
  , NP.Seq 1660 1662
  , NP.Var 1661 "NOP_1661"
  , NP.Seq 1661 1662
  , NP.Var 1662 "IF_ELSE_FOOTER"
  , NP.Assign 1663 "self_or_null" (NP.Num 0)
  , NP.Seq 1663 1664
  , NP.Var 1664 "callable_o"
  , NP.Seq 1664 1665
  , NP.Assign 1665 "self_or_null" (NP.Num 0)
  , NP.Seq 1665 1666
  , NP.Var 1666 "temp"
  , NP.Seq 1666 1667
  , NP.Assign 1667 "callable" (NP.Num 0)
  , NP.Seq 1667 1668
  , NP.Assign 1668 "undefed" (NP.Num 0)
  , NP.Seq 1668 1669
  , NP.Assign 1669 "undefed" (NP.Num 0)
  , NP.Seq 1669 1670
  , NP.Assign 1670 "stack_pointer" (NP.Num 0)
  , NP.Seq 1670 1671
  , NP.Var 1671 "func_version"
  , NP.Seq 1671 1672
  , NP.Var 1672 "callable_o"
  , NP.Seq 1672 1673
  , NP.Branch 1673 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Plus (NP.Id "PyFunction_Type") (NP.Num 0))) (NP.Num 0)) (NP.Num 1)) 1675 1675
  , NP.Seq 1674 1310
  , NP.Seq 1674 1676
  , NP.Var 1675 "NOP_1675"
  , NP.Seq 1675 1676
  , NP.Var 1676 "IF_ELSE_FOOTER"
  , NP.Var 1677 "func"
  , NP.Seq 1677 1678
  , NP.Branch 1678 (NP.Eq (NP.Plus (NP.Num 0) (NP.Id "func_version")) (NP.Num 1)) 1680 1680
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
  , NP.Branch 1685 (NP.Eq (NP.Plus (NP.Num 0) (NP.Plus (NP.Id "oparg") (NP.Plus (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 0)))) (NP.Num 1)) 1687 1687
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
  , NP.Branch 1692 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 1694 1694
  , NP.Seq 1693 1310
  , NP.Seq 1693 1695
  , NP.Var 1694 "NOP_1694"
  , NP.Seq 1694 1695
  , NP.Var 1695 "IF_ELSE_FOOTER"
  , NP.Branch 1696 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 1698 1698
  , NP.Seq 1697 1310
  , NP.Seq 1697 1699
  , NP.Var 1698 "NOP_1698"
  , NP.Seq 1698 1699
  , NP.Var 1699 "IF_ELSE_FOOTER"
  , NP.Assign 1700 "args" (NP.Num 0)
  , NP.Seq 1700 1701
  , NP.Var 1701 "has_self"
  , NP.Seq 1701 1702
  , NP.Var 1702 "pushed_frame"
  , NP.Seq 1702 1703
  , NP.Var 1703 "first_non_self_local"
  , NP.Seq 1703 1704
  , NP.Assign 1704 "undefed" (NP.Num 0)
  , NP.Seq 1704 1705
  , NP.Var 1705 "i"
  , NP.Seq 1705 1706
  , NP.Branch 1706 (NP.Eq (NP.Plus (NP.Id "i") (NP.Id "oparg")) (NP.Num 1)) 1707 1708
  , NP.Assign 1707 "undefed" (NP.Num 0)
  , NP.Seq 1707 1708
  , NP.Seq 1707 1706
  , NP.Var 1708 "LOOP_FOOTER"
  , NP.Seq 1708 1709
  , NP.Assign 1709 "new_frame" (NP.Num 0)
  , NP.Seq 1709 1710
  , NP.Assign 1710 "undefed" (NP.Num 0)
  , NP.Seq 1710 1711
  , NP.Var 1711 "temp"
  , NP.Seq 1711 1712
  , NP.Assign 1712 "stack_pointer" (NP.Num 0)
  , NP.Seq 1712 1713
  , NP.Assign 1713 "frame" (NP.Num 0)
  , NP.Seq 1713 1714
  , NP.Assign 1714 "stack_pointer" (NP.Num 0)
  , NP.Seq 1714 1715
  , NP.Assign 1715 "next_instr" (NP.Num 0)
  , NP.Seq 1715 1716
  , NP.Branch 1716 (NP.Eq (NP.Num 0) (NP.Num 1)) 1717 1718
  , NP.Assign 1717 "next_instr" (NP.Num 0)
  , NP.Seq 1717 1716
  , NP.Var 1718 "LOOP_FOOTER"
  , NP.Seq 1718 1719
  , NP.Var 1719 "word"
  , NP.Seq 1719 1720
  , NP.Assign 1720 "opcode" (NP.Num 0)
  , NP.Seq 1720 1721
  , NP.Assign 1721 "oparg" (NP.Num 0)
  , NP.Seq 1721 1722
  , NP.Branch 1722 (NP.Eq (NP.Num 0) (NP.Num 1)) 1723 1726
  , NP.Var 1723 "word"
  , NP.Seq 1723 1724
  , NP.Assign 1724 "opcode" (NP.Num 0)
  , NP.Seq 1724 1725
  , NP.Assign 1725 "oparg" (NP.Num 0)
  , NP.Seq 1725 1726
  , NP.Seq 1725 1722
  , NP.Var 1726 "LOOP_FOOTER"
  , NP.Seq 1726 1727
  , NP.Seq 1726 35
  , NP.Branch 1727 (NP.Eq (NP.Plus (NP.Id "opcode") (NP.Num 0)) (NP.Num 1)) 1729 1810
  , NP.Var 1729 "NOP_1729"
  , NP.Var 1730 "__CLABEL_TARGET_CALL_BOUND_METHOD_GENERAL"
  , NP.Seq 1730 1731
  , NP.Var 1731 "this_instr"
  , NP.Seq 1731 1732
  , NP.Assign 1732 "undefed" (NP.Num 0)
  , NP.Seq 1732 1733
  , NP.Assign 1733 "next_instr" (NP.Num 0)
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
  , NP.Branch 1739 (NP.Eq (NP.Num 0) (NP.Num 1)) 1741 1741
  , NP.Seq 1740 1310
  , NP.Seq 1740 1742
  , NP.Var 1741 "NOP_1741"
  , NP.Seq 1741 1742
  , NP.Var 1742 "IF_ELSE_FOOTER"
  , NP.Assign 1743 "null" (NP.Num 0)
  , NP.Seq 1743 1744
  , NP.Assign 1744 "callable" (NP.Num 0)
  , NP.Seq 1744 1745
  , NP.Var 1745 "func_version"
  , NP.Seq 1745 1746
  , NP.Var 1746 "callable_o"
  , NP.Seq 1746 1747
  , NP.Branch 1747 (NP.Eq (NP.Plus (NP.Num 0) (NP.Plus (NP.Id "PyMethod_Type") (NP.Num 0))) (NP.Num 1)) 1749 1749
  , NP.Seq 1748 1310
  , NP.Seq 1748 1750
  , NP.Var 1749 "NOP_1749"
  , NP.Seq 1749 1750
  , NP.Var 1750 "IF_ELSE_FOOTER"
  , NP.Var 1751 "func"
  , NP.Seq 1751 1752
  , NP.Branch 1752 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Plus (NP.Id "PyFunction_Type") (NP.Num 0))) (NP.Num 0)) (NP.Num 1)) 1754 1754
  , NP.Seq 1753 1310
  , NP.Seq 1753 1755
  , NP.Var 1754 "NOP_1754"
  , NP.Seq 1754 1755
  , NP.Var 1755 "IF_ELSE_FOOTER"
  , NP.Branch 1756 (NP.Eq (NP.Plus (NP.Num 0) (NP.Id "func_version")) (NP.Num 1)) 1758 1758
  , NP.Seq 1757 1310
  , NP.Seq 1757 1759
  , NP.Var 1758 "NOP_1758"
  , NP.Seq 1758 1759
  , NP.Var 1759 "IF_ELSE_FOOTER"
  , NP.Branch 1760 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 0)) (NP.Num 1)) 1762 1762
  , NP.Seq 1761 1310
  , NP.Seq 1761 1763
  , NP.Var 1762 "NOP_1762"
  , NP.Seq 1762 1763
  , NP.Var 1763 "IF_ELSE_FOOTER"
  , NP.Assign 1764 "self_or_null" (NP.Num 0)
  , NP.Seq 1764 1765
  , NP.Var 1765 "callable_o"
  , NP.Seq 1765 1766
  , NP.Assign 1766 "self_or_null" (NP.Num 0)
  , NP.Seq 1766 1767
  , NP.Var 1767 "temp"
  , NP.Seq 1767 1768
  , NP.Assign 1768 "callable" (NP.Num 0)
  , NP.Seq 1768 1769
  , NP.Assign 1769 "undefed" (NP.Num 0)
  , NP.Seq 1769 1770
  , NP.Assign 1770 "undefed" (NP.Num 0)
  , NP.Seq 1770 1771
  , NP.Assign 1771 "stack_pointer" (NP.Num 0)
  , NP.Seq 1771 1772
  , NP.Branch 1772 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 1774 1774
  , NP.Seq 1773 1310
  , NP.Seq 1773 1775
  , NP.Var 1774 "NOP_1774"
  , NP.Seq 1774 1775
  , NP.Var 1775 "IF_ELSE_FOOTER"
  , NP.Assign 1776 "args" (NP.Num 0)
  , NP.Seq 1776 1777
  , NP.Var 1777 "callable_o"
  , NP.Seq 1777 1778
  , NP.Var 1778 "total_args"
  , NP.Seq 1778 1779
  , NP.Branch 1779 (NP.Eq (NP.Plus (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 0)) (NP.Num 1)) 1781 1782
  , NP.Assign 1781 "total_args" (NP.Num 0)
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
  , NP.Assign 1787 "stack_pointer" (NP.Num 0)
  , NP.Seq 1787 1788
  , NP.Assign 1788 "stack_pointer" (NP.Num 0)
  , NP.Seq 1788 1789
  , NP.Branch 1789 (NP.Eq (NP.Plus (NP.Id "temp") (NP.Num 0)) (NP.Num 1)) 1791 1791
  , NP.Seq 1790 1817
  , NP.Seq 1790 1792
  , NP.Var 1791 "NOP_1791"
  , NP.Seq 1791 1792
  , NP.Var 1792 "IF_ELSE_FOOTER"
  , NP.Assign 1793 "new_frame" (NP.Num 0)
  , NP.Seq 1793 1794
  , NP.Assign 1794 "undefed" (NP.Num 0)
  , NP.Seq 1794 1795
  , NP.Var 1795 "temp"
  , NP.Seq 1795 1796
  , NP.Assign 1796 "frame" (NP.Num 0)
  , NP.Seq 1796 1797
  , NP.Assign 1797 "stack_pointer" (NP.Num 0)
  , NP.Seq 1797 1798
  , NP.Assign 1798 "next_instr" (NP.Num 0)
  , NP.Seq 1798 1799
  , NP.Branch 1799 (NP.Eq (NP.Num 0) (NP.Num 1)) 1800 1801
  , NP.Assign 1800 "next_instr" (NP.Num 0)
  , NP.Seq 1800 1799
  , NP.Var 1801 "LOOP_FOOTER"
  , NP.Seq 1801 1802
  , NP.Var 1802 "word"
  , NP.Seq 1802 1803
  , NP.Assign 1803 "opcode" (NP.Num 0)
  , NP.Seq 1803 1804
  , NP.Assign 1804 "oparg" (NP.Num 0)
  , NP.Seq 1804 1805
  , NP.Branch 1805 (NP.Eq (NP.Num 0) (NP.Num 1)) 1806 1809
  , NP.Var 1806 "word"
  , NP.Seq 1806 1807
  , NP.Assign 1807 "opcode" (NP.Num 0)
  , NP.Seq 1807 1808
  , NP.Assign 1808 "oparg" (NP.Num 0)
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
  , NP.Assign 1814 "stack_pointer" (NP.Num 0)
  , NP.Seq 1814 1815
  , NP.Seq 1814 1817
  , NP.Var 1815 "__CLABEL_pop_1_error"
  , NP.Seq 1815 1816
  , NP.Assign 1816 "stack_pointer" (NP.Num 0)
  , NP.Seq 1816 1817
  , NP.Seq 1816 1817
  , NP.Var 1817 "__CLABEL_error"
  , NP.Seq 1817 1818
  , NP.Branch 1818 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 1820 1821
  , NP.Assign 1820 "stack_pointer" (NP.Num 0)
  , NP.Seq 1820 1822
  , NP.Var 1821 "NOP_1821"
  , NP.Seq 1821 1822
  , NP.Var 1822 "IF_ELSE_FOOTER"
  , NP.Branch 1823 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 1825 1832
  , NP.Var 1825 "f"
  , NP.Seq 1825 1826
  , NP.Assign 1826 "stack_pointer" (NP.Num 0)
  , NP.Seq 1826 1827
  , NP.Branch 1827 (NP.Eq (NP.Plus (NP.Id "f") (NP.Num 0)) (NP.Num 1)) 1829 1830
  , NP.Assign 1829 "stack_pointer" (NP.Num 0)
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
  , NP.Branch 1840 (NP.Eq (NP.Plus (NP.Id "handled") (NP.Num 0)) (NP.Num 1)) 1842 1846
  , NP.Var 1842 "stackbase"
  , NP.Seq 1842 1843
  , NP.Branch 1843 (NP.Eq (NP.Plus (NP.Num 0) (NP.Id "stackbase")) (NP.Num 1)) 1844 1845
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
  , NP.Branch 1849 (NP.Eq (NP.Plus (NP.Num 0) (NP.Id "new_top")) (NP.Num 1)) 1850 1851
  , NP.Var 1850 "ref"
  , NP.Seq 1850 1851
  , NP.Seq 1850 1849
  , NP.Var 1851 "LOOP_FOOTER"
  , NP.Seq 1851 1852
  , NP.Branch 1852 (NP.Eq (NP.Id "lasti") (NP.Num 1)) 1854 1856
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
  , NP.Assign 1859 "next_instr" (NP.Num 0)
  , NP.Seq 1859 1860
  , NP.Var 1860 "err"
  , NP.Seq 1860 1861
  , NP.Branch 1861 (NP.Eq (NP.Plus (NP.Id "err") (NP.Num 0)) (NP.Num 1)) 1863 1863
  , NP.Seq 1862 1834
  , NP.Seq 1862 1864
  , NP.Var 1863 "NOP_1863"
  , NP.Seq 1863 1864
  , NP.Var 1864 "IF_ELSE_FOOTER"
  , NP.Assign 1865 "stack_pointer" (NP.Num 0)
  , NP.Seq 1865 1866
  , NP.Var 1866 "word"
  , NP.Seq 1866 1867
  , NP.Assign 1867 "opcode" (NP.Num 0)
  , NP.Seq 1867 1868
  , NP.Assign 1868 "oparg" (NP.Num 0)
  , NP.Seq 1868 1869
  , NP.Branch 1869 (NP.Eq (NP.Num 0) (NP.Num 1)) 1870 1873
  , NP.Var 1870 "word"
  , NP.Seq 1870 1871
  , NP.Assign 1871 "opcode" (NP.Num 0)
  , NP.Seq 1871 1872
  , NP.Assign 1872 "oparg" (NP.Num 0)
  , NP.Seq 1872 1873
  , NP.Seq 1872 1869
  , NP.Var 1873 "LOOP_FOOTER"
  , NP.Seq 1873 1874
  , NP.Seq 1873 35
  , NP.Var 1874 "__CLABEL_exit_unwind"
  , NP.Seq 1874 1875
  , NP.Var 1875 "dying"
  , NP.Seq 1875 1876
  , NP.Assign 1876 "frame" (NP.Num 0)
  , NP.Seq 1876 1877
  , NP.Assign 1877 "undefed" (NP.Num 0)
  , NP.Seq 1877 1878
  , NP.Branch 1878 (NP.Eq (NP.Plus (NP.Num 0) (NP.Id "FRAME_OWNED_BY_INTERPRETER")) (NP.Num 1)) 1880 1882
  , NP.Assign 1880 "undefed" (NP.Num 0)
  , NP.Seq 1880 1881
  , NP.Assign 1881 "return" (NP.Num 0)
  , NP.Seq 1881 1882
  , NP.Seq 1881 1883
  , NP.Var 1882 "NOP_1882"
  , NP.Seq 1882 1883
  , NP.Var 1883 "IF_ELSE_FOOTER"
  , NP.Assign 1884 "next_instr" (NP.Num 0)
  , NP.Seq 1884 1885
  , NP.Assign 1885 "stack_pointer" (NP.Num 0)
  , NP.Seq 1885 1886
  , NP.Seq 1885 1817
  , NP.Var 1886 "__CLABEL_start_frame"
  , NP.Seq 1886 1887
  , NP.Var 1887 "too_deep"
  , NP.Seq 1887 1888
  , NP.Branch 1888 (NP.Eq (NP.Id "too_deep") (NP.Num 1)) 1890 1890
  , NP.Seq 1889 1874
  , NP.Seq 1889 1891
  , NP.Var 1890 "NOP_1890"
  , NP.Seq 1890 1891
  , NP.Var 1891 "IF_ELSE_FOOTER"
  , NP.Assign 1892 "next_instr" (NP.Num 0)
  , NP.Seq 1892 1893
  , NP.Assign 1893 "stack_pointer" (NP.Num 0)
  , NP.Seq 1893 1894
  , NP.Var 1894 "word"
  , NP.Seq 1894 1895
  , NP.Assign 1895 "opcode" (NP.Num 0)
  , NP.Seq 1895 1896
  , NP.Assign 1896 "oparg" (NP.Num 0)
  , NP.Seq 1896 1897
  , NP.Branch 1897 (NP.Eq (NP.Num 0) (NP.Num 1)) 1898 1901
  , NP.Var 1898 "word"
  , NP.Seq 1898 1899
  , NP.Assign 1899 "opcode" (NP.Num 0)
  , NP.Seq 1899 1900
  , NP.Assign 1900 "oparg" (NP.Num 0)
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
  , NP.Assign 1905 "frame" (NP.Num 0)
  , NP.Seq 1905 1906
  , NP.Assign 1906 "undefed" (NP.Num 0)
  , NP.Seq 1906 1907
  , NP.Assign 1907 "undefed" (NP.Num 0)
  , NP.Seq 1907 1908
  , NP.Assign 1908 "return" (NP.Num 0)
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
  , WP.Branch 7 (WP.Eq (WP.Num 0) (WP.Num 1)) 9 10
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
  , WP.Branch 25 (WP.Eq (WP.Id "throwflag") (WP.Num 1)) 27 33
  , WP.Branch 27 (WP.Eq (WP.Num 0) (WP.Num 1)) 29 29
  , WP.Seq 28 1902
  , WP.Seq 28 30
  , WP.Var 29 "NOP_29"
  , WP.Seq 29 30
  , WP.Var 30 "IF_ELSE_FOOTER"
  , WP.Assign 31 "next_instr" (WP.Num 0)
  , WP.Seq 31 32
  , WP.Assign 32 "stack_pointer" (WP.Num 0)
  , WP.Seq 32 33
  , WP.Seq 32 1817
  , WP.Seq 32 34
  , WP.Var 33 "NOP_33"
  , WP.Seq 33 34
  , WP.Var 34 "IF_ELSE_FOOTER"
  , WP.Seq 34 1886
  , WP.Var 35 "__CLABEL_dispatch_opcode"
  , WP.Seq 35 36
  , WP.Branch 36 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 38 87
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
  , WP.Branch 51 (WP.Eq (WP.Num 0) (WP.Num 1)) 53 56
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
  , WP.Branch 59 (WP.Eq (WP.Num 0) (WP.Num 1)) 60 61
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
  , WP.Branch 66 (WP.Eq (WP.Plus (WP.Id "res_o") (WP.Num 0)) (WP.Num 1)) 68 68
  , WP.Seq 67 1817
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
  , WP.Branch 82 (WP.Eq (WP.Num 0) (WP.Num 1)) 83 86
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
  , WP.Branch 87 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 89 129
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
  , WP.Branch 100 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Plus (WP.Id "PyFloat_Type") (WP.Num 0))) (WP.Num 0)) (WP.Num 1)) 102 102
  , WP.Seq 101 42
  , WP.Seq 101 103
  , WP.Var 102 "NOP_102"
  , WP.Seq 102 103
  , WP.Var 103 "IF_ELSE_FOOTER"
  , WP.Assign 104 "left" (WP.Num 0)
  , WP.Seq 104 105
  , WP.Var 105 "left_o"
  , WP.Seq 105 106
  , WP.Branch 106 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Plus (WP.Id "PyFloat_Type") (WP.Num 0))) (WP.Num 0)) (WP.Num 1)) 108 108
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
  , WP.Branch 115 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 117 117
  , WP.Seq 116 1813
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
  , WP.Branch 124 (WP.Eq (WP.Num 0) (WP.Num 1)) 125 128
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
  , WP.Branch 129 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 131 170
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
  , WP.Branch 142 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 144 144
  , WP.Seq 143 42
  , WP.Seq 143 145
  , WP.Var 144 "NOP_144"
  , WP.Seq 144 145
  , WP.Var 145 "IF_ELSE_FOOTER"
  , WP.Assign 146 "left" (WP.Num 0)
  , WP.Seq 146 147
  , WP.Var 147 "left_o"
  , WP.Seq 147 148
  , WP.Branch 148 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 150 150
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
  , WP.Branch 156 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 158 158
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
  , WP.Branch 165 (WP.Eq (WP.Num 0) (WP.Num 1)) 166 169
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
  , WP.Branch 170 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 172 214
  , WP.Var 172 "NOP_172"
  , WP.Var 173 "__CLABEL_TARGET_BINARY_OP_ADD_UNICODE"
  , WP.Seq 173 174
  , WP.Var 174 "this_instr"
  , WP.Seq 174 175
  , WP.Assign 175 "undefed" (WP.Num 0)
  , WP.Seq 175 176
  , WP.Assign 176 "next_instr" (WP.Num 0)
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
  , WP.Assign 182 "value" (WP.Num 0)
  , WP.Seq 182 183
  , WP.Var 183 "value_o"
  , WP.Seq 183 184
  , WP.Branch 184 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Plus (WP.Id "PyUnicode_Type") (WP.Num 0))) (WP.Num 0)) (WP.Num 1)) 186 186
  , WP.Seq 185 42
  , WP.Seq 185 187
  , WP.Var 186 "NOP_186"
  , WP.Seq 186 187
  , WP.Var 187 "IF_ELSE_FOOTER"
  , WP.Assign 188 "nos" (WP.Num 0)
  , WP.Seq 188 189
  , WP.Var 189 "o"
  , WP.Seq 189 190
  , WP.Branch 190 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Plus (WP.Id "PyUnicode_Type") (WP.Num 0))) (WP.Num 0)) (WP.Num 1)) 192 192
  , WP.Seq 191 42
  , WP.Seq 191 193
  , WP.Var 192 "NOP_192"
  , WP.Seq 192 193
  , WP.Var 193 "IF_ELSE_FOOTER"
  , WP.Assign 194 "right" (WP.Num 0)
  , WP.Seq 194 195
  , WP.Assign 195 "left" (WP.Num 0)
  , WP.Seq 195 196
  , WP.Var 196 "left_o"
  , WP.Seq 196 197
  , WP.Var 197 "right_o"
  , WP.Seq 197 198
  , WP.Var 198 "res_o"
  , WP.Seq 198 199
  , WP.Branch 199 (WP.Eq (WP.Plus (WP.Id "res_o") (WP.Num 0)) (WP.Num 1)) 201 201
  , WP.Seq 200 1813
  , WP.Seq 200 202
  , WP.Var 201 "NOP_201"
  , WP.Seq 201 202
  , WP.Var 202 "IF_ELSE_FOOTER"
  , WP.Assign 203 "res" (WP.Num 0)
  , WP.Seq 203 204
  , WP.Assign 204 "undefed" (WP.Num 0)
  , WP.Seq 204 205
  , WP.Assign 205 "stack_pointer" (WP.Num 0)
  , WP.Seq 205 206
  , WP.Var 206 "word"
  , WP.Seq 206 207
  , WP.Assign 207 "opcode" (WP.Num 0)
  , WP.Seq 207 208
  , WP.Assign 208 "oparg" (WP.Num 0)
  , WP.Seq 208 209
  , WP.Branch 209 (WP.Eq (WP.Num 0) (WP.Num 1)) 210 213
  , WP.Var 210 "word"
  , WP.Seq 210 211
  , WP.Assign 211 "opcode" (WP.Num 0)
  , WP.Seq 211 212
  , WP.Assign 212 "oparg" (WP.Num 0)
  , WP.Seq 212 213
  , WP.Seq 212 209
  , WP.Var 213 "LOOP_FOOTER"
  , WP.Seq 213 214
  , WP.Seq 213 35
  , WP.Branch 214 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 216 260
  , WP.Var 216 "NOP_216"
  , WP.Var 217 "__CLABEL_TARGET_BINARY_OP_EXTEND"
  , WP.Seq 217 218
  , WP.Var 218 "this_instr"
  , WP.Seq 218 219
  , WP.Assign 219 "undefed" (WP.Num 0)
  , WP.Seq 219 220
  , WP.Assign 220 "next_instr" (WP.Num 0)
  , WP.Seq 220 221
  , WP.Var 221 "left"
  , WP.Seq 221 222
  , WP.Var 222 "right"
  , WP.Seq 222 223
  , WP.Var 223 "res"
  , WP.Seq 223 224
  , WP.Assign 224 "right" (WP.Num 0)
  , WP.Seq 224 225
  , WP.Assign 225 "left" (WP.Num 0)
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
  , WP.Assign 231 "stack_pointer" (WP.Num 0)
  , WP.Seq 231 232
  , WP.Branch 232 (WP.Eq (WP.Plus (WP.Id "res") (WP.Num 0)) (WP.Num 1)) 234 234
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
  , WP.Assign 242 "right" (WP.Num 0)
  , WP.Seq 242 243
  , WP.Assign 243 "undefed" (WP.Num 0)
  , WP.Seq 243 244
  , WP.Assign 244 "tmp" (WP.Num 0)
  , WP.Seq 244 245
  , WP.Assign 245 "left" (WP.Num 0)
  , WP.Seq 245 246
  , WP.Assign 246 "undefed" (WP.Num 0)
  , WP.Seq 246 247
  , WP.Assign 247 "stack_pointer" (WP.Num 0)
  , WP.Seq 247 248
  , WP.Assign 248 "stack_pointer" (WP.Num 0)
  , WP.Seq 248 249
  , WP.Assign 249 "res" (WP.Num 0)
  , WP.Seq 249 250
  , WP.Assign 250 "undefed" (WP.Num 0)
  , WP.Seq 250 251
  , WP.Assign 251 "stack_pointer" (WP.Num 0)
  , WP.Seq 251 252
  , WP.Var 252 "word"
  , WP.Seq 252 253
  , WP.Assign 253 "opcode" (WP.Num 0)
  , WP.Seq 253 254
  , WP.Assign 254 "oparg" (WP.Num 0)
  , WP.Seq 254 255
  , WP.Branch 255 (WP.Eq (WP.Num 0) (WP.Num 1)) 256 259
  , WP.Var 256 "word"
  , WP.Seq 256 257
  , WP.Assign 257 "opcode" (WP.Num 0)
  , WP.Seq 257 258
  , WP.Assign 258 "oparg" (WP.Num 0)
  , WP.Seq 258 259
  , WP.Seq 258 255
  , WP.Var 259 "LOOP_FOOTER"
  , WP.Seq 259 260
  , WP.Seq 259 35
  , WP.Branch 260 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 262 354
  , WP.Var 262 "NOP_262"
  , WP.Var 263 "__CLABEL_TARGET_BINARY_OP_INPLACE_ADD_UNICODE"
  , WP.Seq 263 264
  , WP.Var 264 "this_instr"
  , WP.Seq 264 265
  , WP.Assign 265 "undefed" (WP.Num 0)
  , WP.Seq 265 266
  , WP.Assign 266 "next_instr" (WP.Num 0)
  , WP.Seq 266 267
  , WP.Var 267 "value"
  , WP.Seq 267 268
  , WP.Var 268 "nos"
  , WP.Seq 268 269
  , WP.Var 269 "left"
  , WP.Seq 269 270
  , WP.Var 270 "right"
  , WP.Seq 270 271
  , WP.Assign 271 "value" (WP.Num 0)
  , WP.Seq 271 272
  , WP.Var 272 "value_o"
  , WP.Seq 272 273
  , WP.Branch 273 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Plus (WP.Id "PyUnicode_Type") (WP.Num 0))) (WP.Num 0)) (WP.Num 1)) 275 275
  , WP.Seq 274 42
  , WP.Seq 274 276
  , WP.Var 275 "NOP_275"
  , WP.Seq 275 276
  , WP.Var 276 "IF_ELSE_FOOTER"
  , WP.Assign 277 "nos" (WP.Num 0)
  , WP.Seq 277 278
  , WP.Var 278 "o"
  , WP.Seq 278 279
  , WP.Branch 279 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Plus (WP.Id "PyUnicode_Type") (WP.Num 0))) (WP.Num 0)) (WP.Num 1)) 281 281
  , WP.Seq 280 42
  , WP.Seq 280 282
  , WP.Var 281 "NOP_281"
  , WP.Seq 281 282
  , WP.Var 282 "IF_ELSE_FOOTER"
  , WP.Assign 283 "right" (WP.Num 0)
  , WP.Seq 283 284
  , WP.Assign 284 "left" (WP.Num 0)
  , WP.Seq 284 285
  , WP.Var 285 "left_o"
  , WP.Seq 285 286
  , WP.Var 286 "next_oparg"
  , WP.Seq 286 287
  , WP.Assign 287 "next_oparg" (WP.Num 0)
  , WP.Seq 287 288
  , WP.Var 288 "target_local"
  , WP.Seq 288 289
  , WP.Branch 289 (WP.Eq (WP.Plus (WP.Num 0) (WP.Id "left_o")) (WP.Num 1)) 291 291
  , WP.Seq 290 42
  , WP.Seq 290 292
  , WP.Var 291 "NOP_291"
  , WP.Seq 291 292
  , WP.Var 292 "IF_ELSE_FOOTER"
  , WP.Var 293 "temp"
  , WP.Seq 293 294
  , WP.Var 294 "right_o"
  , WP.Seq 294 295
  , WP.Assign 295 "stack_pointer" (WP.Num 0)
  , WP.Seq 295 296
  , WP.Assign 296 "stack_pointer" (WP.Num 0)
  , WP.Seq 296 297
  , WP.Assign 297 "undefed" (WP.Num 0)
  , WP.Seq 297 298
  , WP.Var 298 "op"
  , WP.Seq 298 299
  , WP.Branch 299 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 0)) (WP.Num 1)) 301 316
  , WP.Var 301 "tracer"
  , WP.Seq 301 302
  , WP.Branch 302 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 304 305
  , WP.Var 304 "data"
  , WP.Seq 304 305
  , WP.Seq 304 306
  , WP.Var 305 "NOP_305"
  , WP.Seq 305 306
  , WP.Var 306 "IF_ELSE_FOOTER"
  , WP.Branch 307 (WP.Eq (WP.Num 0) (WP.Num 1)) 308 314
  , WP.Var 308 "tracer"
  , WP.Seq 308 309
  , WP.Branch 309 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 311 312
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
  , WP.Branch 318 (WP.Eq (WP.Num 0) (WP.Num 1)) 319 339
  , WP.Var 319 "op"
  , WP.Seq 319 320
  , WP.Branch 320 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 0)) (WP.Num 1)) 322 337
  , WP.Var 322 "tracer"
  , WP.Seq 322 323
  , WP.Branch 323 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 325 326
  , WP.Var 325 "data"
  , WP.Seq 325 326
  , WP.Seq 325 327
  , WP.Var 326 "NOP_326"
  , WP.Seq 326 327
  , WP.Var 327 "IF_ELSE_FOOTER"
  , WP.Branch 328 (WP.Eq (WP.Num 0) (WP.Num 1)) 329 335
  , WP.Var 329 "tracer"
  , WP.Seq 329 330
  , WP.Branch 330 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 332 333
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
  , WP.Assign 340 "stack_pointer" (WP.Num 0)
  , WP.Seq 340 341
  , WP.Branch 341 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 343 343
  , WP.Seq 342 1817
  , WP.Seq 342 344
  , WP.Var 343 "NOP_343"
  , WP.Seq 343 344
  , WP.Var 344 "IF_ELSE_FOOTER"
  , WP.Assign 345 "next_instr" (WP.Num 0)
  , WP.Seq 345 346
  , WP.Var 346 "word"
  , WP.Seq 346 347
  , WP.Assign 347 "opcode" (WP.Num 0)
  , WP.Seq 347 348
  , WP.Assign 348 "oparg" (WP.Num 0)
  , WP.Seq 348 349
  , WP.Branch 349 (WP.Eq (WP.Num 0) (WP.Num 1)) 350 353
  , WP.Var 350 "word"
  , WP.Seq 350 351
  , WP.Assign 351 "opcode" (WP.Num 0)
  , WP.Seq 351 352
  , WP.Assign 352 "oparg" (WP.Num 0)
  , WP.Seq 352 353
  , WP.Seq 352 349
  , WP.Var 353 "LOOP_FOOTER"
  , WP.Seq 353 354
  , WP.Seq 353 35
  , WP.Branch 354 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 356 396
  , WP.Var 356 "NOP_356"
  , WP.Var 357 "__CLABEL_TARGET_BINARY_OP_MULTIPLY_FLOAT"
  , WP.Seq 357 358
  , WP.Var 358 "this_instr"
  , WP.Seq 358 359
  , WP.Assign 359 "undefed" (WP.Num 0)
  , WP.Seq 359 360
  , WP.Assign 360 "next_instr" (WP.Num 0)
  , WP.Seq 360 361
  , WP.Var 361 "value"
  , WP.Seq 361 362
  , WP.Var 362 "left"
  , WP.Seq 362 363
  , WP.Var 363 "right"
  , WP.Seq 363 364
  , WP.Var 364 "res"
  , WP.Seq 364 365
  , WP.Assign 365 "value" (WP.Num 0)
  , WP.Seq 365 366
  , WP.Var 366 "value_o"
  , WP.Seq 366 367
  , WP.Branch 367 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Plus (WP.Id "PyFloat_Type") (WP.Num 0))) (WP.Num 0)) (WP.Num 1)) 369 369
  , WP.Seq 368 42
  , WP.Seq 368 370
  , WP.Var 369 "NOP_369"
  , WP.Seq 369 370
  , WP.Var 370 "IF_ELSE_FOOTER"
  , WP.Assign 371 "left" (WP.Num 0)
  , WP.Seq 371 372
  , WP.Var 372 "left_o"
  , WP.Seq 372 373
  , WP.Branch 373 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Plus (WP.Id "PyFloat_Type") (WP.Num 0))) (WP.Num 0)) (WP.Num 1)) 375 375
  , WP.Seq 374 42
  , WP.Seq 374 376
  , WP.Var 375 "NOP_375"
  , WP.Seq 375 376
  , WP.Var 376 "IF_ELSE_FOOTER"
  , WP.Assign 377 "right" (WP.Num 0)
  , WP.Seq 377 378
  , WP.Var 378 "left_o"
  , WP.Seq 378 379
  , WP.Var 379 "right_o"
  , WP.Seq 379 380
  , WP.Var 380 "dres"
  , WP.Seq 380 381
  , WP.Assign 381 "res" (WP.Num 0)
  , WP.Seq 381 382
  , WP.Branch 382 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 384 384
  , WP.Seq 383 1813
  , WP.Seq 383 385
  , WP.Var 384 "NOP_384"
  , WP.Seq 384 385
  , WP.Var 385 "IF_ELSE_FOOTER"
  , WP.Assign 386 "undefed" (WP.Num 0)
  , WP.Seq 386 387
  , WP.Assign 387 "stack_pointer" (WP.Num 0)
  , WP.Seq 387 388
  , WP.Var 388 "word"
  , WP.Seq 388 389
  , WP.Assign 389 "opcode" (WP.Num 0)
  , WP.Seq 389 390
  , WP.Assign 390 "oparg" (WP.Num 0)
  , WP.Seq 390 391
  , WP.Branch 391 (WP.Eq (WP.Num 0) (WP.Num 1)) 392 395
  , WP.Var 392 "word"
  , WP.Seq 392 393
  , WP.Assign 393 "opcode" (WP.Num 0)
  , WP.Seq 393 394
  , WP.Assign 394 "oparg" (WP.Num 0)
  , WP.Seq 394 395
  , WP.Seq 394 391
  , WP.Var 395 "LOOP_FOOTER"
  , WP.Seq 395 396
  , WP.Seq 395 35
  , WP.Branch 396 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 398 437
  , WP.Var 398 "NOP_398"
  , WP.Var 399 "__CLABEL_TARGET_BINARY_OP_MULTIPLY_INT"
  , WP.Seq 399 400
  , WP.Var 400 "this_instr"
  , WP.Seq 400 401
  , WP.Assign 401 "undefed" (WP.Num 0)
  , WP.Seq 401 402
  , WP.Assign 402 "next_instr" (WP.Num 0)
  , WP.Seq 402 403
  , WP.Var 403 "value"
  , WP.Seq 403 404
  , WP.Var 404 "left"
  , WP.Seq 404 405
  , WP.Var 405 "right"
  , WP.Seq 405 406
  , WP.Var 406 "res"
  , WP.Seq 406 407
  , WP.Assign 407 "value" (WP.Num 0)
  , WP.Seq 407 408
  , WP.Var 408 "value_o"
  , WP.Seq 408 409
  , WP.Branch 409 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 411 411
  , WP.Seq 410 42
  , WP.Seq 410 412
  , WP.Var 411 "NOP_411"
  , WP.Seq 411 412
  , WP.Var 412 "IF_ELSE_FOOTER"
  , WP.Assign 413 "left" (WP.Num 0)
  , WP.Seq 413 414
  , WP.Var 414 "left_o"
  , WP.Seq 414 415
  , WP.Branch 415 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 417 417
  , WP.Seq 416 42
  , WP.Seq 416 418
  , WP.Var 417 "NOP_417"
  , WP.Seq 417 418
  , WP.Var 418 "IF_ELSE_FOOTER"
  , WP.Assign 419 "right" (WP.Num 0)
  , WP.Seq 419 420
  , WP.Var 420 "left_o"
  , WP.Seq 420 421
  , WP.Var 421 "right_o"
  , WP.Seq 421 422
  , WP.Assign 422 "res" (WP.Num 0)
  , WP.Seq 422 423
  , WP.Branch 423 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 425 425
  , WP.Seq 424 42
  , WP.Seq 424 426
  , WP.Var 425 "NOP_425"
  , WP.Seq 425 426
  , WP.Var 426 "IF_ELSE_FOOTER"
  , WP.Assign 427 "undefed" (WP.Num 0)
  , WP.Seq 427 428
  , WP.Assign 428 "stack_pointer" (WP.Num 0)
  , WP.Seq 428 429
  , WP.Var 429 "word"
  , WP.Seq 429 430
  , WP.Assign 430 "opcode" (WP.Num 0)
  , WP.Seq 430 431
  , WP.Assign 431 "oparg" (WP.Num 0)
  , WP.Seq 431 432
  , WP.Branch 432 (WP.Eq (WP.Num 0) (WP.Num 1)) 433 436
  , WP.Var 433 "word"
  , WP.Seq 433 434
  , WP.Assign 434 "opcode" (WP.Num 0)
  , WP.Seq 434 435
  , WP.Assign 435 "oparg" (WP.Num 0)
  , WP.Seq 435 436
  , WP.Seq 435 432
  , WP.Var 436 "LOOP_FOOTER"
  , WP.Seq 436 437
  , WP.Seq 436 35
  , WP.Branch 437 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 439 489
  , WP.Var 439 "NOP_439"
  , WP.Var 440 "__CLABEL_TARGET_BINARY_OP_SUBSCR_DICT"
  , WP.Seq 440 441
  , WP.Var 441 "this_instr"
  , WP.Seq 441 442
  , WP.Assign 442 "undefed" (WP.Num 0)
  , WP.Seq 442 443
  , WP.Assign 443 "next_instr" (WP.Num 0)
  , WP.Seq 443 444
  , WP.Var 444 "nos"
  , WP.Seq 444 445
  , WP.Var 445 "dict_st"
  , WP.Seq 445 446
  , WP.Var 446 "sub_st"
  , WP.Seq 446 447
  , WP.Var 447 "res"
  , WP.Seq 447 448
  , WP.Assign 448 "nos" (WP.Num 0)
  , WP.Seq 448 449
  , WP.Var 449 "o"
  , WP.Seq 449 450
  , WP.Branch 450 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Plus (WP.Id "PyDict_Type") (WP.Num 0))) (WP.Num 0)) (WP.Num 1)) 452 452
  , WP.Seq 451 42
  , WP.Seq 451 453
  , WP.Var 452 "NOP_452"
  , WP.Seq 452 453
  , WP.Var 453 "IF_ELSE_FOOTER"
  , WP.Assign 454 "sub_st" (WP.Num 0)
  , WP.Seq 454 455
  , WP.Assign 455 "dict_st" (WP.Num 0)
  , WP.Seq 455 456
  , WP.Var 456 "sub"
  , WP.Seq 456 457
  , WP.Var 457 "dict"
  , WP.Seq 457 458
  , WP.Var 458 "res_o"
  , WP.Seq 458 459
  , WP.Var 459 "rc"
  , WP.Seq 459 460
  , WP.Assign 460 "stack_pointer" (WP.Num 0)
  , WP.Seq 460 461
  , WP.Branch 461 (WP.Eq (WP.Plus (WP.Id "rc") (WP.Num 0)) (WP.Num 1)) 463 464
  , WP.Assign 463 "stack_pointer" (WP.Num 0)
  , WP.Seq 463 465
  , WP.Var 464 "NOP_464"
  , WP.Seq 464 465
  , WP.Var 465 "IF_ELSE_FOOTER"
  , WP.Var 466 "tmp"
  , WP.Seq 466 467
  , WP.Assign 467 "sub_st" (WP.Num 0)
  , WP.Seq 467 468
  , WP.Assign 468 "undefed" (WP.Num 0)
  , WP.Seq 468 469
  , WP.Assign 469 "tmp" (WP.Num 0)
  , WP.Seq 469 470
  , WP.Assign 470 "dict_st" (WP.Num 0)
  , WP.Seq 470 471
  , WP.Assign 471 "undefed" (WP.Num 0)
  , WP.Seq 471 472
  , WP.Assign 472 "stack_pointer" (WP.Num 0)
  , WP.Seq 472 473
  , WP.Assign 473 "stack_pointer" (WP.Num 0)
  , WP.Seq 473 474
  , WP.Branch 474 (WP.Eq (WP.Plus (WP.Id "rc") (WP.Num 0)) (WP.Num 1)) 476 476
  , WP.Seq 475 1817
  , WP.Seq 475 477
  , WP.Var 476 "NOP_476"
  , WP.Seq 476 477
  , WP.Var 477 "IF_ELSE_FOOTER"
  , WP.Assign 478 "res" (WP.Num 0)
  , WP.Seq 478 479
  , WP.Assign 479 "undefed" (WP.Num 0)
  , WP.Seq 479 480
  , WP.Assign 480 "stack_pointer" (WP.Num 0)
  , WP.Seq 480 481
  , WP.Var 481 "word"
  , WP.Seq 481 482
  , WP.Assign 482 "opcode" (WP.Num 0)
  , WP.Seq 482 483
  , WP.Assign 483 "oparg" (WP.Num 0)
  , WP.Seq 483 484
  , WP.Branch 484 (WP.Eq (WP.Num 0) (WP.Num 1)) 485 488
  , WP.Var 485 "word"
  , WP.Seq 485 486
  , WP.Assign 486 "opcode" (WP.Num 0)
  , WP.Seq 486 487
  , WP.Assign 487 "oparg" (WP.Num 0)
  , WP.Seq 487 488
  , WP.Seq 487 484
  , WP.Var 488 "LOOP_FOOTER"
  , WP.Seq 488 489
  , WP.Seq 488 35
  , WP.Branch 489 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 491 549
  , WP.Var 491 "NOP_491"
  , WP.Var 492 "__CLABEL_TARGET_BINARY_OP_SUBSCR_GETITEM"
  , WP.Seq 492 493
  , WP.Var 493 "this_instr"
  , WP.Seq 493 494
  , WP.Assign 494 "undefed" (WP.Num 0)
  , WP.Seq 494 495
  , WP.Assign 495 "next_instr" (WP.Num 0)
  , WP.Seq 495 496
  , WP.Var 496 "container"
  , WP.Seq 496 497
  , WP.Var 497 "getitem"
  , WP.Seq 497 498
  , WP.Var 498 "sub"
  , WP.Seq 498 499
  , WP.Var 499 "new_frame"
  , WP.Seq 499 500
  , WP.Branch 500 (WP.Eq (WP.Num 0) (WP.Num 1)) 502 502
  , WP.Seq 501 42
  , WP.Seq 501 503
  , WP.Var 502 "NOP_502"
  , WP.Seq 502 503
  , WP.Var 503 "IF_ELSE_FOOTER"
  , WP.Assign 504 "container" (WP.Num 0)
  , WP.Seq 504 505
  , WP.Var 505 "tp"
  , WP.Seq 505 506
  , WP.Branch 506 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 508 508
  , WP.Seq 507 42
  , WP.Seq 507 509
  , WP.Var 508 "NOP_508"
  , WP.Seq 508 509
  , WP.Var 509 "IF_ELSE_FOOTER"
  , WP.Var 510 "ht"
  , WP.Seq 510 511
  , WP.Var 511 "getitem_o"
  , WP.Seq 511 512
  , WP.Branch 512 (WP.Eq (WP.Plus (WP.Id "getitem_o") (WP.Num 0)) (WP.Num 1)) 514 514
  , WP.Seq 513 42
  , WP.Seq 513 515
  , WP.Var 514 "NOP_514"
  , WP.Seq 514 515
  , WP.Var 515 "IF_ELSE_FOOTER"
  , WP.Var 516 "cached_version"
  , WP.Seq 516 517
  , WP.Branch 517 (WP.Eq (WP.Plus (WP.Num 0) (WP.Id "cached_version")) (WP.Num 1)) 519 519
  , WP.Seq 518 42
  , WP.Seq 518 520
  , WP.Var 519 "NOP_519"
  , WP.Seq 519 520
  , WP.Var 520 "IF_ELSE_FOOTER"
  , WP.Var 521 "code"
  , WP.Seq 521 522
  , WP.Branch 522 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 524 524
  , WP.Seq 523 42
  , WP.Seq 523 525
  , WP.Var 524 "NOP_524"
  , WP.Seq 524 525
  , WP.Var 525 "IF_ELSE_FOOTER"
  , WP.Assign 526 "getitem" (WP.Num 0)
  , WP.Seq 526 527
  , WP.Assign 527 "sub" (WP.Num 0)
  , WP.Seq 527 528
  , WP.Var 528 "pushed_frame"
  , WP.Seq 528 529
  , WP.Assign 529 "undefed" (WP.Num 0)
  , WP.Seq 529 530
  , WP.Assign 530 "undefed" (WP.Num 0)
  , WP.Seq 530 531
  , WP.Assign 531 "undefed" (WP.Num 0)
  , WP.Seq 531 532
  , WP.Assign 532 "new_frame" (WP.Num 0)
  , WP.Seq 532 533
  , WP.Var 533 "temp"
  , WP.Seq 533 534
  , WP.Assign 534 "stack_pointer" (WP.Num 0)
  , WP.Seq 534 535
  , WP.Assign 535 "frame" (WP.Num 0)
  , WP.Seq 535 536
  , WP.Assign 536 "stack_pointer" (WP.Num 0)
  , WP.Seq 536 537
  , WP.Assign 537 "next_instr" (WP.Num 0)
  , WP.Seq 537 538
  , WP.Branch 538 (WP.Eq (WP.Num 0) (WP.Num 1)) 539 540
  , WP.Assign 539 "next_instr" (WP.Num 0)
  , WP.Seq 539 538
  , WP.Var 540 "LOOP_FOOTER"
  , WP.Seq 540 541
  , WP.Var 541 "word"
  , WP.Seq 541 542
  , WP.Assign 542 "opcode" (WP.Num 0)
  , WP.Seq 542 543
  , WP.Assign 543 "oparg" (WP.Num 0)
  , WP.Seq 543 544
  , WP.Branch 544 (WP.Eq (WP.Num 0) (WP.Num 1)) 545 548
  , WP.Var 545 "word"
  , WP.Seq 545 546
  , WP.Assign 546 "opcode" (WP.Num 0)
  , WP.Seq 546 547
  , WP.Assign 547 "oparg" (WP.Num 0)
  , WP.Seq 547 548
  , WP.Seq 547 544
  , WP.Var 548 "LOOP_FOOTER"
  , WP.Seq 548 549
  , WP.Seq 548 35
  , WP.Branch 549 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 551 604
  , WP.Var 551 "NOP_551"
  , WP.Var 552 "__CLABEL_TARGET_BINARY_OP_SUBSCR_LIST_INT"
  , WP.Seq 552 553
  , WP.Var 553 "this_instr"
  , WP.Seq 553 554
  , WP.Assign 554 "undefed" (WP.Num 0)
  , WP.Seq 554 555
  , WP.Assign 555 "next_instr" (WP.Num 0)
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
  , WP.Assign 561 "value" (WP.Num 0)
  , WP.Seq 561 562
  , WP.Var 562 "value_o"
  , WP.Seq 562 563
  , WP.Branch 563 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 565 565
  , WP.Seq 564 42
  , WP.Seq 564 566
  , WP.Var 565 "NOP_565"
  , WP.Seq 565 566
  , WP.Var 566 "IF_ELSE_FOOTER"
  , WP.Assign 567 "nos" (WP.Num 0)
  , WP.Seq 567 568
  , WP.Var 568 "o"
  , WP.Seq 568 569
  , WP.Branch 569 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Plus (WP.Id "PyList_Type") (WP.Num 0))) (WP.Num 0)) (WP.Num 1)) 571 571
  , WP.Seq 570 42
  , WP.Seq 570 572
  , WP.Var 571 "NOP_571"
  , WP.Seq 571 572
  , WP.Var 572 "IF_ELSE_FOOTER"
  , WP.Assign 573 "sub_st" (WP.Num 0)
  , WP.Seq 573 574
  , WP.Assign 574 "list_st" (WP.Num 0)
  , WP.Seq 574 575
  , WP.Var 575 "sub"
  , WP.Seq 575 576
  , WP.Var 576 "list"
  , WP.Seq 576 577
  , WP.Branch 577 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 579 579
  , WP.Seq 578 42
  , WP.Seq 578 580
  , WP.Var 579 "NOP_579"
  , WP.Seq 579 580
  , WP.Var 580 "IF_ELSE_FOOTER"
  , WP.Var 581 "index"
  , WP.Seq 581 582
  , WP.Branch 582 (WP.Eq (WP.Plus (WP.Id "index") (WP.Num 0)) (WP.Num 1)) 584 584
  , WP.Seq 583 42
  , WP.Seq 583 585
  , WP.Var 584 "NOP_584"
  , WP.Seq 584 585
  , WP.Var 585 "IF_ELSE_FOOTER"
  , WP.Var 586 "res_o"
  , WP.Seq 586 587
  , WP.Assign 587 "res" (WP.Num 0)
  , WP.Seq 587 588
  , WP.Var 588 "tmp"
  , WP.Seq 588 589
  , WP.Assign 589 "list_st" (WP.Num 0)
  , WP.Seq 589 590
  , WP.Assign 590 "undefed" (WP.Num 0)
  , WP.Seq 590 591
  , WP.Assign 591 "tmp" (WP.Num 0)
  , WP.Seq 591 592
  , WP.Assign 592 "sub_st" (WP.Num 0)
  , WP.Seq 592 593
  , WP.Assign 593 "undefed" (WP.Num 0)
  , WP.Seq 593 594
  , WP.Assign 594 "stack_pointer" (WP.Num 0)
  , WP.Seq 594 595
  , WP.Assign 595 "stack_pointer" (WP.Num 0)
  , WP.Seq 595 596
  , WP.Var 596 "word"
  , WP.Seq 596 597
  , WP.Assign 597 "opcode" (WP.Num 0)
  , WP.Seq 597 598
  , WP.Assign 598 "oparg" (WP.Num 0)
  , WP.Seq 598 599
  , WP.Branch 599 (WP.Eq (WP.Num 0) (WP.Num 1)) 600 603
  , WP.Var 600 "word"
  , WP.Seq 600 601
  , WP.Assign 601 "opcode" (WP.Num 0)
  , WP.Seq 601 602
  , WP.Assign 602 "oparg" (WP.Num 0)
  , WP.Seq 602 603
  , WP.Seq 602 599
  , WP.Var 603 "LOOP_FOOTER"
  , WP.Seq 603 604
  , WP.Seq 603 35
  , WP.Branch 604 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 606 657
  , WP.Var 606 "NOP_606"
  , WP.Var 607 "__CLABEL_TARGET_BINARY_OP_SUBSCR_LIST_SLICE"
  , WP.Seq 607 608
  , WP.Var 608 "this_instr"
  , WP.Seq 608 609
  , WP.Assign 609 "undefed" (WP.Num 0)
  , WP.Seq 609 610
  , WP.Assign 610 "next_instr" (WP.Num 0)
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
  , WP.Assign 616 "tos" (WP.Num 0)
  , WP.Seq 616 617
  , WP.Var 617 "o"
  , WP.Seq 617 618
  , WP.Branch 618 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Plus (WP.Id "PySlice_Type") (WP.Num 0))) (WP.Num 0)) (WP.Num 1)) 620 620
  , WP.Seq 619 42
  , WP.Seq 619 621
  , WP.Var 620 "NOP_620"
  , WP.Seq 620 621
  , WP.Var 621 "IF_ELSE_FOOTER"
  , WP.Assign 622 "nos" (WP.Num 0)
  , WP.Seq 622 623
  , WP.Var 623 "o"
  , WP.Seq 623 624
  , WP.Branch 624 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Plus (WP.Id "PyList_Type") (WP.Num 0))) (WP.Num 0)) (WP.Num 1)) 626 626
  , WP.Seq 625 42
  , WP.Seq 625 627
  , WP.Var 626 "NOP_626"
  , WP.Seq 626 627
  , WP.Var 627 "IF_ELSE_FOOTER"
  , WP.Assign 628 "sub_st" (WP.Num 0)
  , WP.Seq 628 629
  , WP.Assign 629 "list_st" (WP.Num 0)
  , WP.Seq 629 630
  , WP.Var 630 "sub"
  , WP.Seq 630 631
  , WP.Var 631 "list"
  , WP.Seq 631 632
  , WP.Var 632 "res_o"
  , WP.Seq 632 633
  , WP.Assign 633 "stack_pointer" (WP.Num 0)
  , WP.Seq 633 634
  , WP.Var 634 "tmp"
  , WP.Seq 634 635
  , WP.Assign 635 "sub_st" (WP.Num 0)
  , WP.Seq 635 636
  , WP.Assign 636 "undefed" (WP.Num 0)
  , WP.Seq 636 637
  , WP.Assign 637 "tmp" (WP.Num 0)
  , WP.Seq 637 638
  , WP.Assign 638 "list_st" (WP.Num 0)
  , WP.Seq 638 639
  , WP.Assign 639 "undefed" (WP.Num 0)
  , WP.Seq 639 640
  , WP.Assign 640 "stack_pointer" (WP.Num 0)
  , WP.Seq 640 641
  , WP.Assign 641 "stack_pointer" (WP.Num 0)
  , WP.Seq 641 642
  , WP.Branch 642 (WP.Eq (WP.Plus (WP.Id "res_o") (WP.Num 0)) (WP.Num 1)) 644 644
  , WP.Seq 643 1817
  , WP.Seq 643 645
  , WP.Var 644 "NOP_644"
  , WP.Seq 644 645
  , WP.Var 645 "IF_ELSE_FOOTER"
  , WP.Assign 646 "res" (WP.Num 0)
  , WP.Seq 646 647
  , WP.Assign 647 "undefed" (WP.Num 0)
  , WP.Seq 647 648
  , WP.Assign 648 "stack_pointer" (WP.Num 0)
  , WP.Seq 648 649
  , WP.Var 649 "word"
  , WP.Seq 649 650
  , WP.Assign 650 "opcode" (WP.Num 0)
  , WP.Seq 650 651
  , WP.Assign 651 "oparg" (WP.Num 0)
  , WP.Seq 651 652
  , WP.Branch 652 (WP.Eq (WP.Num 0) (WP.Num 1)) 653 656
  , WP.Var 653 "word"
  , WP.Seq 653 654
  , WP.Assign 654 "opcode" (WP.Num 0)
  , WP.Seq 654 655
  , WP.Assign 655 "oparg" (WP.Num 0)
  , WP.Seq 655 656
  , WP.Seq 655 652
  , WP.Var 656 "LOOP_FOOTER"
  , WP.Seq 656 657
  , WP.Seq 656 35
  , WP.Branch 657 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 659 713
  , WP.Var 659 "NOP_659"
  , WP.Var 660 "__CLABEL_TARGET_BINARY_OP_SUBSCR_STR_INT"
  , WP.Seq 660 661
  , WP.Var 661 "this_instr"
  , WP.Seq 661 662
  , WP.Assign 662 "undefed" (WP.Num 0)
  , WP.Seq 662 663
  , WP.Assign 663 "next_instr" (WP.Num 0)
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
  , WP.Assign 669 "value" (WP.Num 0)
  , WP.Seq 669 670
  , WP.Var 670 "value_o"
  , WP.Seq 670 671
  , WP.Branch 671 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 673 673
  , WP.Seq 672 42
  , WP.Seq 672 674
  , WP.Var 673 "NOP_673"
  , WP.Seq 673 674
  , WP.Var 674 "IF_ELSE_FOOTER"
  , WP.Assign 675 "nos" (WP.Num 0)
  , WP.Seq 675 676
  , WP.Var 676 "o"
  , WP.Seq 676 677
  , WP.Branch 677 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Plus (WP.Id "PyUnicode_Type") (WP.Num 0))) (WP.Num 0)) (WP.Num 1)) 679 679
  , WP.Seq 678 42
  , WP.Seq 678 680
  , WP.Var 679 "NOP_679"
  , WP.Seq 679 680
  , WP.Var 680 "IF_ELSE_FOOTER"
  , WP.Assign 681 "sub_st" (WP.Num 0)
  , WP.Seq 681 682
  , WP.Assign 682 "str_st" (WP.Num 0)
  , WP.Seq 682 683
  , WP.Var 683 "sub"
  , WP.Seq 683 684
  , WP.Var 684 "str"
  , WP.Seq 684 685
  , WP.Branch 685 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 687 687
  , WP.Seq 686 42
  , WP.Seq 686 688
  , WP.Var 687 "NOP_687"
  , WP.Seq 687 688
  , WP.Var 688 "IF_ELSE_FOOTER"
  , WP.Var 689 "index"
  , WP.Seq 689 690
  , WP.Branch 690 (WP.Eq (WP.Plus (WP.Num 0) (WP.Id "index")) (WP.Num 1)) 692 692
  , WP.Seq 691 42
  , WP.Seq 691 693
  , WP.Var 692 "NOP_692"
  , WP.Seq 692 693
  , WP.Var 693 "IF_ELSE_FOOTER"
  , WP.Var 694 "c"
  , WP.Seq 694 695
  , WP.Branch 695 (WP.Eq (WP.Plus (WP.Plus (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Plus (WP.Num 0) (WP.Num 0))) (WP.Id "c")) (WP.Num 1)) 697 697
  , WP.Seq 696 42
  , WP.Seq 696 698
  , WP.Var 697 "NOP_697"
  , WP.Seq 697 698
  , WP.Var 698 "IF_ELSE_FOOTER"
  , WP.Var 699 "res_o"
  , WP.Seq 699 700
  , WP.Assign 700 "stack_pointer" (WP.Num 0)
  , WP.Seq 700 701
  , WP.Assign 701 "stack_pointer" (WP.Num 0)
  , WP.Seq 701 702
  , WP.Assign 702 "res" (WP.Num 0)
  , WP.Seq 702 703
  , WP.Assign 703 "undefed" (WP.Num 0)
  , WP.Seq 703 704
  , WP.Assign 704 "stack_pointer" (WP.Num 0)
  , WP.Seq 704 705
  , WP.Var 705 "word"
  , WP.Seq 705 706
  , WP.Assign 706 "opcode" (WP.Num 0)
  , WP.Seq 706 707
  , WP.Assign 707 "oparg" (WP.Num 0)
  , WP.Seq 707 708
  , WP.Branch 708 (WP.Eq (WP.Num 0) (WP.Num 1)) 709 712
  , WP.Var 709 "word"
  , WP.Seq 709 710
  , WP.Assign 710 "opcode" (WP.Num 0)
  , WP.Seq 710 711
  , WP.Assign 711 "oparg" (WP.Num 0)
  , WP.Seq 711 712
  , WP.Seq 711 708
  , WP.Var 712 "LOOP_FOOTER"
  , WP.Seq 712 713
  , WP.Seq 712 35
  , WP.Branch 713 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 715 765
  , WP.Var 715 "NOP_715"
  , WP.Var 716 "__CLABEL_TARGET_BINARY_OP_SUBSCR_TUPLE_INT"
  , WP.Seq 716 717
  , WP.Var 717 "this_instr"
  , WP.Seq 717 718
  , WP.Assign 718 "undefed" (WP.Num 0)
  , WP.Seq 718 719
  , WP.Assign 719 "next_instr" (WP.Num 0)
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
  , WP.Assign 725 "value" (WP.Num 0)
  , WP.Seq 725 726
  , WP.Var 726 "value_o"
  , WP.Seq 726 727
  , WP.Branch 727 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 729 729
  , WP.Seq 728 42
  , WP.Seq 728 730
  , WP.Var 729 "NOP_729"
  , WP.Seq 729 730
  , WP.Var 730 "IF_ELSE_FOOTER"
  , WP.Assign 731 "nos" (WP.Num 0)
  , WP.Seq 731 732
  , WP.Var 732 "o"
  , WP.Seq 732 733
  , WP.Branch 733 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Plus (WP.Id "PyTuple_Type") (WP.Num 0))) (WP.Num 0)) (WP.Num 1)) 735 735
  , WP.Seq 734 42
  , WP.Seq 734 736
  , WP.Var 735 "NOP_735"
  , WP.Seq 735 736
  , WP.Var 736 "IF_ELSE_FOOTER"
  , WP.Assign 737 "sub_st" (WP.Num 0)
  , WP.Seq 737 738
  , WP.Assign 738 "tuple_st" (WP.Num 0)
  , WP.Seq 738 739
  , WP.Var 739 "sub"
  , WP.Seq 739 740
  , WP.Var 740 "tuple"
  , WP.Seq 740 741
  , WP.Branch 741 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 743 743
  , WP.Seq 742 42
  , WP.Seq 742 744
  , WP.Var 743 "NOP_743"
  , WP.Seq 743 744
  , WP.Var 744 "IF_ELSE_FOOTER"
  , WP.Var 745 "index"
  , WP.Seq 745 746
  , WP.Branch 746 (WP.Eq (WP.Plus (WP.Id "index") (WP.Num 0)) (WP.Num 1)) 748 748
  , WP.Seq 747 42
  , WP.Seq 747 749
  , WP.Var 748 "NOP_748"
  , WP.Seq 748 749
  , WP.Var 749 "IF_ELSE_FOOTER"
  , WP.Var 750 "res_o"
  , WP.Seq 750 751
  , WP.Assign 751 "res" (WP.Num 0)
  , WP.Seq 751 752
  , WP.Assign 752 "stack_pointer" (WP.Num 0)
  , WP.Seq 752 753
  , WP.Var 753 "tmp"
  , WP.Seq 753 754
  , WP.Assign 754 "tuple_st" (WP.Num 0)
  , WP.Seq 754 755
  , WP.Assign 755 "undefed" (WP.Num 0)
  , WP.Seq 755 756
  , WP.Assign 756 "stack_pointer" (WP.Num 0)
  , WP.Seq 756 757
  , WP.Var 757 "word"
  , WP.Seq 757 758
  , WP.Assign 758 "opcode" (WP.Num 0)
  , WP.Seq 758 759
  , WP.Assign 759 "oparg" (WP.Num 0)
  , WP.Seq 759 760
  , WP.Branch 760 (WP.Eq (WP.Num 0) (WP.Num 1)) 761 764
  , WP.Var 761 "word"
  , WP.Seq 761 762
  , WP.Assign 762 "opcode" (WP.Num 0)
  , WP.Seq 762 763
  , WP.Assign 763 "oparg" (WP.Num 0)
  , WP.Seq 763 764
  , WP.Seq 763 760
  , WP.Var 764 "LOOP_FOOTER"
  , WP.Seq 764 765
  , WP.Seq 764 35
  , WP.Branch 765 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 767 807
  , WP.Var 767 "NOP_767"
  , WP.Var 768 "__CLABEL_TARGET_BINARY_OP_SUBTRACT_FLOAT"
  , WP.Seq 768 769
  , WP.Var 769 "this_instr"
  , WP.Seq 769 770
  , WP.Assign 770 "undefed" (WP.Num 0)
  , WP.Seq 770 771
  , WP.Assign 771 "next_instr" (WP.Num 0)
  , WP.Seq 771 772
  , WP.Var 772 "value"
  , WP.Seq 772 773
  , WP.Var 773 "left"
  , WP.Seq 773 774
  , WP.Var 774 "right"
  , WP.Seq 774 775
  , WP.Var 775 "res"
  , WP.Seq 775 776
  , WP.Assign 776 "value" (WP.Num 0)
  , WP.Seq 776 777
  , WP.Var 777 "value_o"
  , WP.Seq 777 778
  , WP.Branch 778 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Plus (WP.Id "PyFloat_Type") (WP.Num 0))) (WP.Num 0)) (WP.Num 1)) 780 780
  , WP.Seq 779 42
  , WP.Seq 779 781
  , WP.Var 780 "NOP_780"
  , WP.Seq 780 781
  , WP.Var 781 "IF_ELSE_FOOTER"
  , WP.Assign 782 "left" (WP.Num 0)
  , WP.Seq 782 783
  , WP.Var 783 "left_o"
  , WP.Seq 783 784
  , WP.Branch 784 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Plus (WP.Id "PyFloat_Type") (WP.Num 0))) (WP.Num 0)) (WP.Num 1)) 786 786
  , WP.Seq 785 42
  , WP.Seq 785 787
  , WP.Var 786 "NOP_786"
  , WP.Seq 786 787
  , WP.Var 787 "IF_ELSE_FOOTER"
  , WP.Assign 788 "right" (WP.Num 0)
  , WP.Seq 788 789
  , WP.Var 789 "left_o"
  , WP.Seq 789 790
  , WP.Var 790 "right_o"
  , WP.Seq 790 791
  , WP.Var 791 "dres"
  , WP.Seq 791 792
  , WP.Assign 792 "res" (WP.Num 0)
  , WP.Seq 792 793
  , WP.Branch 793 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 795 795
  , WP.Seq 794 1813
  , WP.Seq 794 796
  , WP.Var 795 "NOP_795"
  , WP.Seq 795 796
  , WP.Var 796 "IF_ELSE_FOOTER"
  , WP.Assign 797 "undefed" (WP.Num 0)
  , WP.Seq 797 798
  , WP.Assign 798 "stack_pointer" (WP.Num 0)
  , WP.Seq 798 799
  , WP.Var 799 "word"
  , WP.Seq 799 800
  , WP.Assign 800 "opcode" (WP.Num 0)
  , WP.Seq 800 801
  , WP.Assign 801 "oparg" (WP.Num 0)
  , WP.Seq 801 802
  , WP.Branch 802 (WP.Eq (WP.Num 0) (WP.Num 1)) 803 806
  , WP.Var 803 "word"
  , WP.Seq 803 804
  , WP.Assign 804 "opcode" (WP.Num 0)
  , WP.Seq 804 805
  , WP.Assign 805 "oparg" (WP.Num 0)
  , WP.Seq 805 806
  , WP.Seq 805 802
  , WP.Var 806 "LOOP_FOOTER"
  , WP.Seq 806 807
  , WP.Seq 806 35
  , WP.Branch 807 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 809 848
  , WP.Var 809 "NOP_809"
  , WP.Var 810 "__CLABEL_TARGET_BINARY_OP_SUBTRACT_INT"
  , WP.Seq 810 811
  , WP.Var 811 "this_instr"
  , WP.Seq 811 812
  , WP.Assign 812 "undefed" (WP.Num 0)
  , WP.Seq 812 813
  , WP.Assign 813 "next_instr" (WP.Num 0)
  , WP.Seq 813 814
  , WP.Var 814 "value"
  , WP.Seq 814 815
  , WP.Var 815 "left"
  , WP.Seq 815 816
  , WP.Var 816 "right"
  , WP.Seq 816 817
  , WP.Var 817 "res"
  , WP.Seq 817 818
  , WP.Assign 818 "value" (WP.Num 0)
  , WP.Seq 818 819
  , WP.Var 819 "value_o"
  , WP.Seq 819 820
  , WP.Branch 820 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 822 822
  , WP.Seq 821 42
  , WP.Seq 821 823
  , WP.Var 822 "NOP_822"
  , WP.Seq 822 823
  , WP.Var 823 "IF_ELSE_FOOTER"
  , WP.Assign 824 "left" (WP.Num 0)
  , WP.Seq 824 825
  , WP.Var 825 "left_o"
  , WP.Seq 825 826
  , WP.Branch 826 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 828 828
  , WP.Seq 827 42
  , WP.Seq 827 829
  , WP.Var 828 "NOP_828"
  , WP.Seq 828 829
  , WP.Var 829 "IF_ELSE_FOOTER"
  , WP.Assign 830 "right" (WP.Num 0)
  , WP.Seq 830 831
  , WP.Var 831 "left_o"
  , WP.Seq 831 832
  , WP.Var 832 "right_o"
  , WP.Seq 832 833
  , WP.Assign 833 "res" (WP.Num 0)
  , WP.Seq 833 834
  , WP.Branch 834 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 836 836
  , WP.Seq 835 42
  , WP.Seq 835 837
  , WP.Var 836 "NOP_836"
  , WP.Seq 836 837
  , WP.Var 837 "IF_ELSE_FOOTER"
  , WP.Assign 838 "undefed" (WP.Num 0)
  , WP.Seq 838 839
  , WP.Assign 839 "stack_pointer" (WP.Num 0)
  , WP.Seq 839 840
  , WP.Var 840 "word"
  , WP.Seq 840 841
  , WP.Assign 841 "opcode" (WP.Num 0)
  , WP.Seq 841 842
  , WP.Assign 842 "oparg" (WP.Num 0)
  , WP.Seq 842 843
  , WP.Branch 843 (WP.Eq (WP.Num 0) (WP.Num 1)) 844 847
  , WP.Var 844 "word"
  , WP.Seq 844 845
  , WP.Assign 845 "opcode" (WP.Num 0)
  , WP.Seq 845 846
  , WP.Assign 846 "oparg" (WP.Num 0)
  , WP.Seq 846 847
  , WP.Seq 846 843
  , WP.Var 847 "LOOP_FOOTER"
  , WP.Seq 847 848
  , WP.Seq 847 35
  , WP.Branch 848 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 850 931
  , WP.Var 850 "NOP_850"
  , WP.Var 851 "__CLABEL_TARGET_BINARY_SLICE"
  , WP.Seq 851 852
  , WP.Assign 852 "undefed" (WP.Num 0)
  , WP.Seq 852 853
  , WP.Assign 853 "next_instr" (WP.Num 0)
  , WP.Seq 853 854
  , WP.Var 854 "container"
  , WP.Seq 854 855
  , WP.Var 855 "start"
  , WP.Seq 855 856
  , WP.Var 856 "stop"
  , WP.Seq 856 857
  , WP.Var 857 "res"
  , WP.Seq 857 858
  , WP.Assign 858 "stop" (WP.Num 0)
  , WP.Seq 858 859
  , WP.Assign 859 "start" (WP.Num 0)
  , WP.Seq 859 860
  , WP.Assign 860 "container" (WP.Num 0)
  , WP.Seq 860 861
  , WP.Var 861 "slice"
  , WP.Seq 861 862
  , WP.Assign 862 "stack_pointer" (WP.Num 0)
  , WP.Seq 862 863
  , WP.Var 863 "res_o"
  , WP.Seq 863 864
  , WP.Branch 864 (WP.Eq (WP.Plus (WP.Id "slice") (WP.Num 0)) (WP.Num 1)) 866 867
  , WP.Assign 866 "res_o" (WP.Num 0)
  , WP.Seq 866 913
  , WP.Assign 867 "stack_pointer" (WP.Num 0)
  , WP.Seq 867 868
  , WP.Assign 868 "res_o" (WP.Num 0)
  , WP.Seq 868 869
  , WP.Var 869 "op"
  , WP.Seq 869 870
  , WP.Branch 870 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 0)) (WP.Num 1)) 872 887
  , WP.Var 872 "tracer"
  , WP.Seq 872 873
  , WP.Branch 873 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 875 876
  , WP.Var 875 "data"
  , WP.Seq 875 876
  , WP.Seq 875 877
  , WP.Var 876 "NOP_876"
  , WP.Seq 876 877
  , WP.Var 877 "IF_ELSE_FOOTER"
  , WP.Branch 878 (WP.Eq (WP.Num 0) (WP.Num 1)) 879 885
  , WP.Var 879 "tracer"
  , WP.Seq 879 880
  , WP.Branch 880 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 882 883
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
  , WP.Branch 889 (WP.Eq (WP.Num 0) (WP.Num 1)) 890 910
  , WP.Var 890 "op"
  , WP.Seq 890 891
  , WP.Branch 891 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 0)) (WP.Num 1)) 893 908
  , WP.Var 893 "tracer"
  , WP.Seq 893 894
  , WP.Branch 894 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 896 897
  , WP.Var 896 "data"
  , WP.Seq 896 897
  , WP.Seq 896 898
  , WP.Var 897 "NOP_897"
  , WP.Seq 897 898
  , WP.Var 898 "IF_ELSE_FOOTER"
  , WP.Branch 899 (WP.Eq (WP.Num 0) (WP.Num 1)) 900 906
  , WP.Var 900 "tracer"
  , WP.Seq 900 901
  , WP.Branch 901 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 903 904
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
  , WP.Assign 911 "stack_pointer" (WP.Num 0)
  , WP.Seq 911 912
  , WP.Assign 912 "stack_pointer" (WP.Num 0)
  , WP.Seq 912 913
  , WP.Var 913 "IF_ELSE_FOOTER"
  , WP.Assign 914 "stack_pointer" (WP.Num 0)
  , WP.Seq 914 915
  , WP.Assign 915 "stack_pointer" (WP.Num 0)
  , WP.Seq 915 916
  , WP.Branch 916 (WP.Eq (WP.Plus (WP.Id "res_o") (WP.Num 0)) (WP.Num 1)) 918 918
  , WP.Seq 917 1817
  , WP.Seq 917 919
  , WP.Var 918 "NOP_918"
  , WP.Seq 918 919
  , WP.Var 919 "IF_ELSE_FOOTER"
  , WP.Assign 920 "res" (WP.Num 0)
  , WP.Seq 920 921
  , WP.Assign 921 "undefed" (WP.Num 0)
  , WP.Seq 921 922
  , WP.Assign 922 "stack_pointer" (WP.Num 0)
  , WP.Seq 922 923
  , WP.Var 923 "word"
  , WP.Seq 923 924
  , WP.Assign 924 "opcode" (WP.Num 0)
  , WP.Seq 924 925
  , WP.Assign 925 "oparg" (WP.Num 0)
  , WP.Seq 925 926
  , WP.Branch 926 (WP.Eq (WP.Num 0) (WP.Num 1)) 927 930
  , WP.Var 927 "word"
  , WP.Seq 927 928
  , WP.Assign 928 "opcode" (WP.Num 0)
  , WP.Seq 928 929
  , WP.Assign 929 "oparg" (WP.Num 0)
  , WP.Seq 929 930
  , WP.Seq 929 926
  , WP.Var 930 "LOOP_FOOTER"
  , WP.Seq 930 931
  , WP.Seq 930 35
  , WP.Branch 931 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 933 980
  , WP.Var 933 "NOP_933"
  , WP.Var 934 "__CLABEL_TARGET_BUILD_INTERPOLATION"
  , WP.Seq 934 935
  , WP.Assign 935 "undefed" (WP.Num 0)
  , WP.Seq 935 936
  , WP.Assign 936 "next_instr" (WP.Num 0)
  , WP.Seq 936 937
  , WP.Var 937 "value"
  , WP.Seq 937 938
  , WP.Var 938 "str"
  , WP.Seq 938 939
  , WP.Var 939 "format"
  , WP.Seq 939 940
  , WP.Var 940 "interpolation"
  , WP.Seq 940 941
  , WP.Assign 941 "format" (WP.Num 0)
  , WP.Seq 941 942
  , WP.Assign 942 "str" (WP.Num 0)
  , WP.Seq 942 943
  , WP.Assign 943 "value" (WP.Num 0)
  , WP.Seq 943 944
  , WP.Var 944 "value_o"
  , WP.Seq 944 945
  , WP.Var 945 "str_o"
  , WP.Seq 945 946
  , WP.Var 946 "conversion"
  , WP.Seq 946 947
  , WP.Var 947 "format_o"
  , WP.Seq 947 948
  , WP.Branch 948 (WP.Eq (WP.Plus (WP.Id "oparg") (WP.Num 0)) (WP.Num 1)) 950 951
  , WP.Assign 950 "format_o" (WP.Num 0)
  , WP.Seq 950 952
  , WP.Assign 951 "format_o" (WP.Num 0)
  , WP.Seq 951 952
  , WP.Var 952 "IF_ELSE_FOOTER"
  , WP.Var 953 "interpolation_o"
  , WP.Seq 953 954
  , WP.Assign 954 "stack_pointer" (WP.Num 0)
  , WP.Seq 954 955
  , WP.Branch 955 (WP.Eq (WP.Plus (WP.Id "oparg") (WP.Num 0)) (WP.Num 1)) 957 959
  , WP.Assign 957 "stack_pointer" (WP.Num 0)
  , WP.Seq 957 958
  , WP.Assign 958 "stack_pointer" (WP.Num 0)
  , WP.Seq 958 959
  , WP.Seq 958 960
  , WP.Assign 959 "stack_pointer" (WP.Num 0)
  , WP.Seq 959 960
  , WP.Var 960 "IF_ELSE_FOOTER"
  , WP.Assign 961 "stack_pointer" (WP.Num 0)
  , WP.Seq 961 962
  , WP.Assign 962 "stack_pointer" (WP.Num 0)
  , WP.Seq 962 963
  , WP.Assign 963 "stack_pointer" (WP.Num 0)
  , WP.Seq 963 964
  , WP.Assign 964 "stack_pointer" (WP.Num 0)
  , WP.Seq 964 965
  , WP.Branch 965 (WP.Eq (WP.Plus (WP.Id "interpolation_o") (WP.Num 0)) (WP.Num 1)) 967 967
  , WP.Seq 966 1817
  , WP.Seq 966 968
  , WP.Var 967 "NOP_967"
  , WP.Seq 967 968
  , WP.Var 968 "IF_ELSE_FOOTER"
  , WP.Assign 969 "interpolation" (WP.Num 0)
  , WP.Seq 969 970
  , WP.Assign 970 "undefed" (WP.Num 0)
  , WP.Seq 970 971
  , WP.Assign 971 "stack_pointer" (WP.Num 0)
  , WP.Seq 971 972
  , WP.Var 972 "word"
  , WP.Seq 972 973
  , WP.Assign 973 "opcode" (WP.Num 0)
  , WP.Seq 973 974
  , WP.Assign 974 "oparg" (WP.Num 0)
  , WP.Seq 974 975
  , WP.Branch 975 (WP.Eq (WP.Num 0) (WP.Num 1)) 976 979
  , WP.Var 976 "word"
  , WP.Seq 976 977
  , WP.Assign 977 "opcode" (WP.Num 0)
  , WP.Seq 977 978
  , WP.Assign 978 "oparg" (WP.Num 0)
  , WP.Seq 978 979
  , WP.Seq 978 975
  , WP.Var 979 "LOOP_FOOTER"
  , WP.Seq 979 980
  , WP.Seq 979 35
  , WP.Branch 980 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 982 1006
  , WP.Var 982 "NOP_982"
  , WP.Var 983 "__CLABEL_TARGET_BUILD_LIST"
  , WP.Seq 983 984
  , WP.Assign 984 "undefed" (WP.Num 0)
  , WP.Seq 984 985
  , WP.Assign 985 "next_instr" (WP.Num 0)
  , WP.Seq 985 986
  , WP.Var 986 "values"
  , WP.Seq 986 987
  , WP.Var 987 "list"
  , WP.Seq 987 988
  , WP.Assign 988 "values" (WP.Num 0)
  , WP.Seq 988 989
  , WP.Var 989 "list_o"
  , WP.Seq 989 990
  , WP.Assign 990 "stack_pointer" (WP.Num 0)
  , WP.Seq 990 991
  , WP.Branch 991 (WP.Eq (WP.Plus (WP.Id "list_o") (WP.Num 0)) (WP.Num 1)) 993 993
  , WP.Seq 992 1817
  , WP.Seq 992 994
  , WP.Var 993 "NOP_993"
  , WP.Seq 993 994
  , WP.Var 994 "IF_ELSE_FOOTER"
  , WP.Assign 995 "list" (WP.Num 0)
  , WP.Seq 995 996
  , WP.Assign 996 "undefed" (WP.Num 0)
  , WP.Seq 996 997
  , WP.Assign 997 "stack_pointer" (WP.Num 0)
  , WP.Seq 997 998
  , WP.Var 998 "word"
  , WP.Seq 998 999
  , WP.Assign 999 "opcode" (WP.Num 0)
  , WP.Seq 999 1000
  , WP.Assign 1000 "oparg" (WP.Num 0)
  , WP.Seq 1000 1001
  , WP.Branch 1001 (WP.Eq (WP.Num 0) (WP.Num 1)) 1002 1005
  , WP.Var 1002 "word"
  , WP.Seq 1002 1003
  , WP.Assign 1003 "opcode" (WP.Num 0)
  , WP.Seq 1003 1004
  , WP.Assign 1004 "oparg" (WP.Num 0)
  , WP.Seq 1004 1005
  , WP.Seq 1004 1001
  , WP.Var 1005 "LOOP_FOOTER"
  , WP.Seq 1005 1006
  , WP.Seq 1005 35
  , WP.Branch 1006 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 1008 1054
  , WP.Var 1008 "NOP_1008"
  , WP.Var 1009 "__CLABEL_TARGET_BUILD_MAP"
  , WP.Seq 1009 1010
  , WP.Assign 1010 "undefed" (WP.Num 0)
  , WP.Seq 1010 1011
  , WP.Assign 1011 "next_instr" (WP.Num 0)
  , WP.Seq 1011 1012
  , WP.Var 1012 "values"
  , WP.Seq 1012 1013
  , WP.Var 1013 "map"
  , WP.Seq 1013 1014
  , WP.Assign 1014 "values" (WP.Num 0)
  , WP.Seq 1014 1015
  , WP.Var 1015 "values_o_temp"
  , WP.Seq 1015 1016
  , WP.Var 1016 "values_o"
  , WP.Seq 1016 1017
  , WP.Branch 1017 (WP.Eq (WP.Plus (WP.Id "values_o") (WP.Num 0)) (WP.Num 1)) 1019 1027
  , WP.Var 1019 "tmp"
  , WP.Seq 1019 1020
  , WP.Var 1020 "_i"
  , WP.Seq 1020 1021
  , WP.Branch 1021 (WP.Eq (WP.Plus (WP.Plus (WP.Id "_i") (WP.Num 0)) (WP.Num 0)) (WP.Num 1)) 1022 1024
  , WP.Assign 1022 "tmp" (WP.Num 0)
  , WP.Seq 1022 1023
  , WP.Assign 1023 "undefed" (WP.Num 0)
  , WP.Seq 1023 1024
  , WP.Seq 1023 1021
  , WP.Var 1024 "LOOP_FOOTER"
  , WP.Seq 1024 1025
  , WP.Assign 1025 "stack_pointer" (WP.Num 0)
  , WP.Seq 1025 1026
  , WP.Assign 1026 "stack_pointer" (WP.Num 0)
  , WP.Seq 1026 1027
  , WP.Seq 1026 1817
  , WP.Seq 1026 1028
  , WP.Var 1027 "NOP_1027"
  , WP.Seq 1027 1028
  , WP.Var 1028 "IF_ELSE_FOOTER"
  , WP.Var 1029 "map_o"
  , WP.Seq 1029 1030
  , WP.Assign 1030 "stack_pointer" (WP.Num 0)
  , WP.Seq 1030 1031
  , WP.Var 1031 "tmp"
  , WP.Seq 1031 1032
  , WP.Var 1032 "_i"
  , WP.Seq 1032 1033
  , WP.Branch 1033 (WP.Eq (WP.Plus (WP.Plus (WP.Id "_i") (WP.Num 0)) (WP.Num 0)) (WP.Num 1)) 1034 1036
  , WP.Assign 1034 "tmp" (WP.Num 0)
  , WP.Seq 1034 1035
  , WP.Assign 1035 "undefed" (WP.Num 0)
  , WP.Seq 1035 1036
  , WP.Seq 1035 1033
  , WP.Var 1036 "LOOP_FOOTER"
  , WP.Seq 1036 1037
  , WP.Assign 1037 "stack_pointer" (WP.Num 0)
  , WP.Seq 1037 1038
  , WP.Assign 1038 "stack_pointer" (WP.Num 0)
  , WP.Seq 1038 1039
  , WP.Branch 1039 (WP.Eq (WP.Plus (WP.Id "map_o") (WP.Num 0)) (WP.Num 1)) 1041 1041
  , WP.Seq 1040 1817
  , WP.Seq 1040 1042
  , WP.Var 1041 "NOP_1041"
  , WP.Seq 1041 1042
  , WP.Var 1042 "IF_ELSE_FOOTER"
  , WP.Assign 1043 "map" (WP.Num 0)
  , WP.Seq 1043 1044
  , WP.Assign 1044 "undefed" (WP.Num 0)
  , WP.Seq 1044 1045
  , WP.Assign 1045 "stack_pointer" (WP.Num 0)
  , WP.Seq 1045 1046
  , WP.Var 1046 "word"
  , WP.Seq 1046 1047
  , WP.Assign 1047 "opcode" (WP.Num 0)
  , WP.Seq 1047 1048
  , WP.Assign 1048 "oparg" (WP.Num 0)
  , WP.Seq 1048 1049
  , WP.Branch 1049 (WP.Eq (WP.Num 0) (WP.Num 1)) 1050 1053
  , WP.Var 1050 "word"
  , WP.Seq 1050 1051
  , WP.Assign 1051 "opcode" (WP.Num 0)
  , WP.Seq 1051 1052
  , WP.Assign 1052 "oparg" (WP.Num 0)
  , WP.Seq 1052 1053
  , WP.Seq 1052 1049
  , WP.Var 1053 "LOOP_FOOTER"
  , WP.Seq 1053 1054
  , WP.Seq 1053 35
  , WP.Branch 1054 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 1056 1148
  , WP.Var 1056 "NOP_1056"
  , WP.Var 1057 "__CLABEL_TARGET_BUILD_SET"
  , WP.Seq 1057 1058
  , WP.Assign 1058 "undefed" (WP.Num 0)
  , WP.Seq 1058 1059
  , WP.Assign 1059 "next_instr" (WP.Num 0)
  , WP.Seq 1059 1060
  , WP.Var 1060 "values"
  , WP.Seq 1060 1061
  , WP.Var 1061 "set"
  , WP.Seq 1061 1062
  , WP.Assign 1062 "values" (WP.Num 0)
  , WP.Seq 1062 1063
  , WP.Var 1063 "set_o"
  , WP.Seq 1063 1064
  , WP.Assign 1064 "stack_pointer" (WP.Num 0)
  , WP.Seq 1064 1065
  , WP.Branch 1065 (WP.Eq (WP.Plus (WP.Id "set_o") (WP.Num 0)) (WP.Num 1)) 1067 1075
  , WP.Var 1067 "tmp"
  , WP.Seq 1067 1068
  , WP.Var 1068 "_i"
  , WP.Seq 1068 1069
  , WP.Branch 1069 (WP.Eq (WP.Plus (WP.Plus (WP.Id "_i") (WP.Num 0)) (WP.Num 0)) (WP.Num 1)) 1070 1072
  , WP.Assign 1070 "tmp" (WP.Num 0)
  , WP.Seq 1070 1071
  , WP.Assign 1071 "undefed" (WP.Num 0)
  , WP.Seq 1071 1072
  , WP.Seq 1071 1069
  , WP.Var 1072 "LOOP_FOOTER"
  , WP.Seq 1072 1073
  , WP.Assign 1073 "stack_pointer" (WP.Num 0)
  , WP.Seq 1073 1074
  , WP.Assign 1074 "stack_pointer" (WP.Num 0)
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
  , WP.Branch 1079 (WP.Eq (WP.Plus (WP.Id "i") (WP.Id "oparg")) (WP.Num 1)) 1080 1088
  , WP.Var 1080 "value"
  , WP.Seq 1080 1081
  , WP.Assign 1081 "undefed" (WP.Num 0)
  , WP.Seq 1081 1082
  , WP.Branch 1082 (WP.Eq (WP.Plus (WP.Id "err") (WP.Num 0)) (WP.Num 1)) 1084 1086
  , WP.Assign 1084 "err" (WP.Num 0)
  , WP.Seq 1084 1085
  , WP.Assign 1085 "stack_pointer" (WP.Num 0)
  , WP.Seq 1085 1086
  , WP.Seq 1085 1087
  , WP.Assign 1086 "stack_pointer" (WP.Num 0)
  , WP.Seq 1086 1087
  , WP.Var 1087 "IF_ELSE_FOOTER"
  , WP.Seq 1087 1079
  , WP.Var 1088 "LOOP_FOOTER"
  , WP.Seq 1088 1089
  , WP.Branch 1089 (WP.Eq (WP.Id "err") (WP.Num 1)) 1091 1135
  , WP.Assign 1091 "stack_pointer" (WP.Num 0)
  , WP.Seq 1091 1092
  , WP.Var 1092 "op"
  , WP.Seq 1092 1093
  , WP.Branch 1093 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 0)) (WP.Num 1)) 1095 1110
  , WP.Var 1095 "tracer"
  , WP.Seq 1095 1096
  , WP.Branch 1096 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 1098 1099
  , WP.Var 1098 "data"
  , WP.Seq 1098 1099
  , WP.Seq 1098 1100
  , WP.Var 1099 "NOP_1099"
  , WP.Seq 1099 1100
  , WP.Var 1100 "IF_ELSE_FOOTER"
  , WP.Branch 1101 (WP.Eq (WP.Num 0) (WP.Num 1)) 1102 1108
  , WP.Var 1102 "tracer"
  , WP.Seq 1102 1103
  , WP.Branch 1103 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 1105 1106
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
  , WP.Branch 1112 (WP.Eq (WP.Num 0) (WP.Num 1)) 1113 1133
  , WP.Var 1113 "op"
  , WP.Seq 1113 1114
  , WP.Branch 1114 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 0)) (WP.Num 1)) 1116 1131
  , WP.Var 1116 "tracer"
  , WP.Seq 1116 1117
  , WP.Branch 1117 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 1119 1120
  , WP.Var 1119 "data"
  , WP.Seq 1119 1120
  , WP.Seq 1119 1121
  , WP.Var 1120 "NOP_1120"
  , WP.Seq 1120 1121
  , WP.Var 1121 "IF_ELSE_FOOTER"
  , WP.Branch 1122 (WP.Eq (WP.Num 0) (WP.Num 1)) 1123 1129
  , WP.Var 1123 "tracer"
  , WP.Seq 1123 1124
  , WP.Branch 1124 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 1126 1127
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
  , WP.Assign 1134 "stack_pointer" (WP.Num 0)
  , WP.Seq 1134 1135
  , WP.Seq 1134 1817
  , WP.Seq 1134 1136
  , WP.Var 1135 "NOP_1135"
  , WP.Seq 1135 1136
  , WP.Var 1136 "IF_ELSE_FOOTER"
  , WP.Assign 1137 "set" (WP.Num 0)
  , WP.Seq 1137 1138
  , WP.Assign 1138 "undefed" (WP.Num 0)
  , WP.Seq 1138 1139
  , WP.Assign 1139 "stack_pointer" (WP.Num 0)
  , WP.Seq 1139 1140
  , WP.Var 1140 "word"
  , WP.Seq 1140 1141
  , WP.Assign 1141 "opcode" (WP.Num 0)
  , WP.Seq 1141 1142
  , WP.Assign 1142 "oparg" (WP.Num 0)
  , WP.Seq 1142 1143
  , WP.Branch 1143 (WP.Eq (WP.Num 0) (WP.Num 1)) 1144 1147
  , WP.Var 1144 "word"
  , WP.Seq 1144 1145
  , WP.Assign 1145 "opcode" (WP.Num 0)
  , WP.Seq 1145 1146
  , WP.Assign 1146 "oparg" (WP.Num 0)
  , WP.Seq 1146 1147
  , WP.Seq 1146 1143
  , WP.Var 1147 "LOOP_FOOTER"
  , WP.Seq 1147 1148
  , WP.Seq 1147 35
  , WP.Branch 1148 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 1150 1184
  , WP.Var 1150 "NOP_1150"
  , WP.Var 1151 "__CLABEL_TARGET_BUILD_SLICE"
  , WP.Seq 1151 1152
  , WP.Assign 1152 "undefed" (WP.Num 0)
  , WP.Seq 1152 1153
  , WP.Assign 1153 "next_instr" (WP.Num 0)
  , WP.Seq 1153 1154
  , WP.Var 1154 "args"
  , WP.Seq 1154 1155
  , WP.Var 1155 "slice"
  , WP.Seq 1155 1156
  , WP.Assign 1156 "args" (WP.Num 0)
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
  , WP.Branch 1163 (WP.Eq (WP.Plus (WP.Plus (WP.Id "_i") (WP.Num 0)) (WP.Num 0)) (WP.Num 1)) 1164 1166
  , WP.Assign 1164 "tmp" (WP.Num 0)
  , WP.Seq 1164 1165
  , WP.Assign 1165 "undefed" (WP.Num 0)
  , WP.Seq 1165 1166
  , WP.Seq 1165 1163
  , WP.Var 1166 "LOOP_FOOTER"
  , WP.Seq 1166 1167
  , WP.Assign 1167 "stack_pointer" (WP.Num 0)
  , WP.Seq 1167 1168
  , WP.Assign 1168 "stack_pointer" (WP.Num 0)
  , WP.Seq 1168 1169
  , WP.Branch 1169 (WP.Eq (WP.Plus (WP.Id "slice_o") (WP.Num 0)) (WP.Num 1)) 1171 1171
  , WP.Seq 1170 1817
  , WP.Seq 1170 1172
  , WP.Var 1171 "NOP_1171"
  , WP.Seq 1171 1172
  , WP.Var 1172 "IF_ELSE_FOOTER"
  , WP.Assign 1173 "slice" (WP.Num 0)
  , WP.Seq 1173 1174
  , WP.Assign 1174 "undefed" (WP.Num 0)
  , WP.Seq 1174 1175
  , WP.Assign 1175 "stack_pointer" (WP.Num 0)
  , WP.Seq 1175 1176
  , WP.Var 1176 "word"
  , WP.Seq 1176 1177
  , WP.Assign 1177 "opcode" (WP.Num 0)
  , WP.Seq 1177 1178
  , WP.Assign 1178 "oparg" (WP.Num 0)
  , WP.Seq 1178 1179
  , WP.Branch 1179 (WP.Eq (WP.Num 0) (WP.Num 1)) 1180 1183
  , WP.Var 1180 "word"
  , WP.Seq 1180 1181
  , WP.Assign 1181 "opcode" (WP.Num 0)
  , WP.Seq 1181 1182
  , WP.Assign 1182 "oparg" (WP.Num 0)
  , WP.Seq 1182 1183
  , WP.Seq 1182 1179
  , WP.Var 1183 "LOOP_FOOTER"
  , WP.Seq 1183 1184
  , WP.Seq 1183 35
  , WP.Branch 1184 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 1186 1231
  , WP.Var 1186 "NOP_1186"
  , WP.Var 1187 "__CLABEL_TARGET_BUILD_STRING"
  , WP.Seq 1187 1188
  , WP.Assign 1188 "undefed" (WP.Num 0)
  , WP.Seq 1188 1189
  , WP.Assign 1189 "next_instr" (WP.Num 0)
  , WP.Seq 1189 1190
  , WP.Var 1190 "pieces"
  , WP.Seq 1190 1191
  , WP.Var 1191 "str"
  , WP.Seq 1191 1192
  , WP.Assign 1192 "pieces" (WP.Num 0)
  , WP.Seq 1192 1193
  , WP.Var 1193 "pieces_o_temp"
  , WP.Seq 1193 1194
  , WP.Var 1194 "pieces_o"
  , WP.Seq 1194 1195
  , WP.Branch 1195 (WP.Eq (WP.Plus (WP.Id "pieces_o") (WP.Num 0)) (WP.Num 1)) 1197 1205
  , WP.Var 1197 "tmp"
  , WP.Seq 1197 1198
  , WP.Var 1198 "_i"
  , WP.Seq 1198 1199
  , WP.Branch 1199 (WP.Eq (WP.Plus (WP.Plus (WP.Id "_i") (WP.Num 0)) (WP.Num 0)) (WP.Num 1)) 1200 1202
  , WP.Assign 1200 "tmp" (WP.Num 0)
  , WP.Seq 1200 1201
  , WP.Assign 1201 "undefed" (WP.Num 0)
  , WP.Seq 1201 1202
  , WP.Seq 1201 1199
  , WP.Var 1202 "LOOP_FOOTER"
  , WP.Seq 1202 1203
  , WP.Assign 1203 "stack_pointer" (WP.Num 0)
  , WP.Seq 1203 1204
  , WP.Assign 1204 "stack_pointer" (WP.Num 0)
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
  , WP.Branch 1210 (WP.Eq (WP.Plus (WP.Plus (WP.Id "_i") (WP.Num 0)) (WP.Num 0)) (WP.Num 1)) 1211 1213
  , WP.Assign 1211 "tmp" (WP.Num 0)
  , WP.Seq 1211 1212
  , WP.Assign 1212 "undefed" (WP.Num 0)
  , WP.Seq 1212 1213
  , WP.Seq 1212 1210
  , WP.Var 1213 "LOOP_FOOTER"
  , WP.Seq 1213 1214
  , WP.Assign 1214 "stack_pointer" (WP.Num 0)
  , WP.Seq 1214 1215
  , WP.Assign 1215 "stack_pointer" (WP.Num 0)
  , WP.Seq 1215 1216
  , WP.Branch 1216 (WP.Eq (WP.Plus (WP.Id "str_o") (WP.Num 0)) (WP.Num 1)) 1218 1218
  , WP.Seq 1217 1817
  , WP.Seq 1217 1219
  , WP.Var 1218 "NOP_1218"
  , WP.Seq 1218 1219
  , WP.Var 1219 "IF_ELSE_FOOTER"
  , WP.Assign 1220 "str" (WP.Num 0)
  , WP.Seq 1220 1221
  , WP.Assign 1221 "undefed" (WP.Num 0)
  , WP.Seq 1221 1222
  , WP.Assign 1222 "stack_pointer" (WP.Num 0)
  , WP.Seq 1222 1223
  , WP.Var 1223 "word"
  , WP.Seq 1223 1224
  , WP.Assign 1224 "opcode" (WP.Num 0)
  , WP.Seq 1224 1225
  , WP.Assign 1225 "oparg" (WP.Num 0)
  , WP.Seq 1225 1226
  , WP.Branch 1226 (WP.Eq (WP.Num 0) (WP.Num 1)) 1227 1230
  , WP.Var 1227 "word"
  , WP.Seq 1227 1228
  , WP.Assign 1228 "opcode" (WP.Num 0)
  , WP.Seq 1228 1229
  , WP.Assign 1229 "oparg" (WP.Num 0)
  , WP.Seq 1229 1230
  , WP.Seq 1229 1226
  , WP.Var 1230 "LOOP_FOOTER"
  , WP.Seq 1230 1231
  , WP.Seq 1230 35
  , WP.Branch 1231 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 1233 1265
  , WP.Var 1233 "NOP_1233"
  , WP.Var 1234 "__CLABEL_TARGET_BUILD_TEMPLATE"
  , WP.Seq 1234 1235
  , WP.Assign 1235 "undefed" (WP.Num 0)
  , WP.Seq 1235 1236
  , WP.Assign 1236 "next_instr" (WP.Num 0)
  , WP.Seq 1236 1237
  , WP.Var 1237 "strings"
  , WP.Seq 1237 1238
  , WP.Var 1238 "interpolations"
  , WP.Seq 1238 1239
  , WP.Var 1239 "template"
  , WP.Seq 1239 1240
  , WP.Assign 1240 "interpolations" (WP.Num 0)
  , WP.Seq 1240 1241
  , WP.Assign 1241 "strings" (WP.Num 0)
  , WP.Seq 1241 1242
  , WP.Var 1242 "strings_o"
  , WP.Seq 1242 1243
  , WP.Var 1243 "interpolations_o"
  , WP.Seq 1243 1244
  , WP.Var 1244 "template_o"
  , WP.Seq 1244 1245
  , WP.Assign 1245 "stack_pointer" (WP.Num 0)
  , WP.Seq 1245 1246
  , WP.Assign 1246 "stack_pointer" (WP.Num 0)
  , WP.Seq 1246 1247
  , WP.Assign 1247 "stack_pointer" (WP.Num 0)
  , WP.Seq 1247 1248
  , WP.Assign 1248 "stack_pointer" (WP.Num 0)
  , WP.Seq 1248 1249
  , WP.Assign 1249 "stack_pointer" (WP.Num 0)
  , WP.Seq 1249 1250
  , WP.Branch 1250 (WP.Eq (WP.Plus (WP.Id "template_o") (WP.Num 0)) (WP.Num 1)) 1252 1252
  , WP.Seq 1251 1817
  , WP.Seq 1251 1253
  , WP.Var 1252 "NOP_1252"
  , WP.Seq 1252 1253
  , WP.Var 1253 "IF_ELSE_FOOTER"
  , WP.Assign 1254 "template" (WP.Num 0)
  , WP.Seq 1254 1255
  , WP.Assign 1255 "undefed" (WP.Num 0)
  , WP.Seq 1255 1256
  , WP.Assign 1256 "stack_pointer" (WP.Num 0)
  , WP.Seq 1256 1257
  , WP.Var 1257 "word"
  , WP.Seq 1257 1258
  , WP.Assign 1258 "opcode" (WP.Num 0)
  , WP.Seq 1258 1259
  , WP.Assign 1259 "oparg" (WP.Num 0)
  , WP.Seq 1259 1260
  , WP.Branch 1260 (WP.Eq (WP.Num 0) (WP.Num 1)) 1261 1264
  , WP.Var 1261 "word"
  , WP.Seq 1261 1262
  , WP.Assign 1262 "opcode" (WP.Num 0)
  , WP.Seq 1262 1263
  , WP.Assign 1263 "oparg" (WP.Num 0)
  , WP.Seq 1263 1264
  , WP.Seq 1263 1260
  , WP.Var 1264 "LOOP_FOOTER"
  , WP.Seq 1264 1265
  , WP.Seq 1264 35
  , WP.Branch 1265 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 1267 1290
  , WP.Var 1267 "NOP_1267"
  , WP.Var 1268 "__CLABEL_TARGET_BUILD_TUPLE"
  , WP.Seq 1268 1269
  , WP.Assign 1269 "undefed" (WP.Num 0)
  , WP.Seq 1269 1270
  , WP.Assign 1270 "next_instr" (WP.Num 0)
  , WP.Seq 1270 1271
  , WP.Var 1271 "values"
  , WP.Seq 1271 1272
  , WP.Var 1272 "tup"
  , WP.Seq 1272 1273
  , WP.Assign 1273 "values" (WP.Num 0)
  , WP.Seq 1273 1274
  , WP.Var 1274 "tup_o"
  , WP.Seq 1274 1275
  , WP.Branch 1275 (WP.Eq (WP.Plus (WP.Id "tup_o") (WP.Num 0)) (WP.Num 1)) 1277 1277
  , WP.Seq 1276 1817
  , WP.Seq 1276 1278
  , WP.Var 1277 "NOP_1277"
  , WP.Seq 1277 1278
  , WP.Var 1278 "IF_ELSE_FOOTER"
  , WP.Assign 1279 "tup" (WP.Num 0)
  , WP.Seq 1279 1280
  , WP.Assign 1280 "undefed" (WP.Num 0)
  , WP.Seq 1280 1281
  , WP.Assign 1281 "stack_pointer" (WP.Num 0)
  , WP.Seq 1281 1282
  , WP.Var 1282 "word"
  , WP.Seq 1282 1283
  , WP.Assign 1283 "opcode" (WP.Num 0)
  , WP.Seq 1283 1284
  , WP.Assign 1284 "oparg" (WP.Num 0)
  , WP.Seq 1284 1285
  , WP.Branch 1285 (WP.Eq (WP.Num 0) (WP.Num 1)) 1286 1289
  , WP.Var 1286 "word"
  , WP.Seq 1286 1287
  , WP.Assign 1287 "opcode" (WP.Num 0)
  , WP.Seq 1287 1288
  , WP.Assign 1288 "oparg" (WP.Num 0)
  , WP.Seq 1288 1289
  , WP.Seq 1288 1285
  , WP.Var 1289 "LOOP_FOOTER"
  , WP.Seq 1289 1290
  , WP.Seq 1289 35
  , WP.Branch 1290 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 1292 1304
  , WP.Var 1292 "NOP_1292"
  , WP.Var 1293 "__CLABEL_TARGET_CACHE"
  , WP.Seq 1293 1294
  , WP.Assign 1294 "undefed" (WP.Num 0)
  , WP.Seq 1294 1295
  , WP.Assign 1295 "next_instr" (WP.Num 0)
  , WP.Seq 1295 1296
  , WP.Var 1296 "word"
  , WP.Seq 1296 1297
  , WP.Assign 1297 "opcode" (WP.Num 0)
  , WP.Seq 1297 1298
  , WP.Assign 1298 "oparg" (WP.Num 0)
  , WP.Seq 1298 1299
  , WP.Branch 1299 (WP.Eq (WP.Num 0) (WP.Num 1)) 1300 1303
  , WP.Var 1300 "word"
  , WP.Seq 1300 1301
  , WP.Assign 1301 "opcode" (WP.Num 0)
  , WP.Seq 1301 1302
  , WP.Assign 1302 "oparg" (WP.Num 0)
  , WP.Seq 1302 1303
  , WP.Seq 1302 1299
  , WP.Var 1303 "LOOP_FOOTER"
  , WP.Seq 1303 1304
  , WP.Seq 1303 35
  , WP.Branch 1304 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 1306 1551
  , WP.Var 1306 "NOP_1306"
  , WP.Var 1307 "__CLABEL_TARGET_CALL"
  , WP.Seq 1307 1308
  , WP.Assign 1308 "undefed" (WP.Num 0)
  , WP.Seq 1308 1309
  , WP.Assign 1309 "next_instr" (WP.Num 0)
  , WP.Seq 1309 1310
  , WP.Var 1310 "__CLABEL_PREDICTED_CALL"
  , WP.Seq 1310 1311
  , WP.Var 1311 "NOP_1311"
  , WP.Var 1312 "this_instr"
  , WP.Seq 1312 1313
  , WP.Assign 1313 "opcode" (WP.Num 0)
  , WP.Seq 1313 1314
  , WP.Var 1314 "callable"
  , WP.Seq 1314 1315
  , WP.Var 1315 "self_or_null"
  , WP.Seq 1315 1316
  , WP.Var 1316 "args"
  , WP.Seq 1316 1317
  , WP.Var 1317 "res"
  , WP.Seq 1317 1318
  , WP.Assign 1318 "self_or_null" (WP.Num 0)
  , WP.Seq 1318 1319
  , WP.Assign 1319 "callable" (WP.Num 0)
  , WP.Seq 1319 1320
  , WP.Var 1320 "counter"
  , WP.Seq 1320 1321
  , WP.Branch 1321 (WP.Eq (WP.Num 0) (WP.Num 1)) 1323 1326
  , WP.Assign 1323 "next_instr" (WP.Num 0)
  , WP.Seq 1323 1324
  , WP.Assign 1324 "stack_pointer" (WP.Num 0)
  , WP.Seq 1324 1325
  , WP.Assign 1325 "opcode" (WP.Num 0)
  , WP.Seq 1325 1326
  , WP.Seq 1325 35
  , WP.Seq 1325 1327
  , WP.Var 1326 "NOP_1326"
  , WP.Seq 1326 1327
  , WP.Var 1327 "IF_ELSE_FOOTER"
  , WP.Assign 1328 "undefed" (WP.Num 0)
  , WP.Seq 1328 1329
  , WP.Branch 1329 (WP.Eq (WP.Num 0) (WP.Num 1)) 1330 1331
  , WP.Assign 1330 "undefed" (WP.Num 0)
  , WP.Seq 1330 1329
  , WP.Var 1331 "LOOP_FOOTER"
  , WP.Seq 1331 1332
  , WP.Branch 1332 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Plus (WP.Id "PyMethod_Type") (WP.Num 0))) (WP.Plus (WP.Num 0) (WP.Num 0))) (WP.Num 1)) 1334 1343
  , WP.Var 1334 "callable_o"
  , WP.Seq 1334 1335
  , WP.Var 1335 "self"
  , WP.Seq 1335 1336
  , WP.Assign 1336 "self_or_null" (WP.Num 0)
  , WP.Seq 1336 1337
  , WP.Var 1337 "method"
  , WP.Seq 1337 1338
  , WP.Var 1338 "temp"
  , WP.Seq 1338 1339
  , WP.Assign 1339 "callable" (WP.Num 0)
  , WP.Seq 1339 1340
  , WP.Assign 1340 "undefed" (WP.Num 0)
  , WP.Seq 1340 1341
  , WP.Assign 1341 "undefed" (WP.Num 0)
  , WP.Seq 1341 1342
  , WP.Assign 1342 "stack_pointer" (WP.Num 0)
  , WP.Seq 1342 1343
  , WP.Seq 1342 1344
  , WP.Var 1343 "NOP_1343"
  , WP.Seq 1343 1344
  , WP.Var 1344 "IF_ELSE_FOOTER"
  , WP.Assign 1345 "args" (WP.Num 0)
  , WP.Seq 1345 1346
  , WP.Var 1346 "callable_o"
  , WP.Seq 1346 1347
  , WP.Var 1347 "total_args"
  , WP.Seq 1347 1348
  , WP.Var 1348 "arguments"
  , WP.Seq 1348 1349
  , WP.Branch 1349 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 0)) (WP.Num 1)) 1351 1352
  , WP.Assign 1351 "total_args" (WP.Num 0)
  , WP.Seq 1351 1352
  , WP.Seq 1351 1353
  , WP.Var 1352 "NOP_1352"
  , WP.Seq 1352 1353
  , WP.Var 1353 "IF_ELSE_FOOTER"
  , WP.Branch 1354 (WP.Eq (WP.Plus (WP.Plus (WP.Plus (WP.Num 0) (WP.Plus (WP.Id "PyFunction_Type") (WP.Num 0))) (WP.Plus (WP.Num 0) (WP.Num 0))) (WP.Plus (WP.Num 0) (WP.Id "_PyFunction_Vectorcall"))) (WP.Num 1)) 1356 1372
  , WP.Var 1356 "code_flags"
  , WP.Seq 1356 1357
  , WP.Var 1357 "locals"
  , WP.Seq 1357 1358
  , WP.Assign 1358 "undefed" (WP.Num 0)
  , WP.Seq 1358 1359
  , WP.Assign 1359 "undefed" (WP.Num 0)
  , WP.Seq 1359 1360
  , WP.Var 1360 "new_frame"
  , WP.Seq 1360 1361
  , WP.Assign 1361 "stack_pointer" (WP.Num 0)
  , WP.Seq 1361 1362
  , WP.Assign 1362 "stack_pointer" (WP.Num 0)
  , WP.Seq 1362 1363
  , WP.Branch 1363 (WP.Eq (WP.Plus (WP.Id "new_frame") (WP.Num 0)) (WP.Num 1)) 1365 1365
  , WP.Seq 1364 1817
  , WP.Seq 1364 1366
  , WP.Var 1365 "NOP_1365"
  , WP.Seq 1365 1366
  , WP.Var 1366 "IF_ELSE_FOOTER"
  , WP.Assign 1367 "undefed" (WP.Num 0)
  , WP.Seq 1367 1368
  , WP.Assign 1368 "frame" (WP.Num 0)
  , WP.Seq 1368 1369
  , WP.Seq 1368 1886
  , WP.Branch 1369 (WP.Eq (WP.Num 0) (WP.Num 1)) 1370 1371
  , WP.Assign 1370 "frame" (WP.Num 0)
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
  , WP.Branch 1376 (WP.Eq (WP.Plus (WP.Id "args_o") (WP.Num 0)) (WP.Num 1)) 1378 1394
  , WP.Var 1378 "tmp"
  , WP.Seq 1378 1379
  , WP.Var 1379 "_i"
  , WP.Seq 1379 1380
  , WP.Branch 1380 (WP.Eq (WP.Plus (WP.Plus (WP.Id "_i") (WP.Num 0)) (WP.Num 0)) (WP.Num 1)) 1381 1385
  , WP.Assign 1381 "tmp" (WP.Num 0)
  , WP.Seq 1381 1382
  , WP.Assign 1382 "undefed" (WP.Num 0)
  , WP.Seq 1382 1383
  , WP.Assign 1383 "undefed" (WP.Num 0)
  , WP.Seq 1383 1384
  , WP.Assign 1384 "undefed" (WP.Num 0)
  , WP.Seq 1384 1385
  , WP.Seq 1384 1380
  , WP.Var 1385 "LOOP_FOOTER"
  , WP.Seq 1385 1386
  , WP.Assign 1386 "tmp" (WP.Num 0)
  , WP.Seq 1386 1387
  , WP.Assign 1387 "self_or_null" (WP.Num 0)
  , WP.Seq 1387 1388
  , WP.Assign 1388 "undefed" (WP.Num 0)
  , WP.Seq 1388 1389
  , WP.Assign 1389 "tmp" (WP.Num 0)
  , WP.Seq 1389 1390
  , WP.Assign 1390 "callable" (WP.Num 0)
  , WP.Seq 1390 1391
  , WP.Assign 1391 "undefed" (WP.Num 0)
  , WP.Seq 1391 1392
  , WP.Assign 1392 "stack_pointer" (WP.Num 0)
  , WP.Seq 1392 1393
  , WP.Assign 1393 "stack_pointer" (WP.Num 0)
  , WP.Seq 1393 1394
  , WP.Seq 1393 1817
  , WP.Seq 1393 1395
  , WP.Var 1394 "NOP_1394"
  , WP.Seq 1394 1395
  , WP.Var 1395 "IF_ELSE_FOOTER"
  , WP.Assign 1396 "undefed" (WP.Num 0)
  , WP.Seq 1396 1397
  , WP.Assign 1397 "undefed" (WP.Num 0)
  , WP.Seq 1397 1398
  , WP.Var 1398 "res_o"
  , WP.Seq 1398 1399
  , WP.Assign 1399 "stack_pointer" (WP.Num 0)
  , WP.Seq 1399 1400
  , WP.Branch 1400 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 1402 1514
  , WP.Var 1402 "arg"
  , WP.Seq 1402 1403
  , WP.Branch 1403 (WP.Eq (WP.Plus (WP.Id "res_o") (WP.Num 0)) (WP.Num 1)) 1405 1406
  , WP.Assign 1405 "stack_pointer" (WP.Num 0)
  , WP.Seq 1405 1513
  , WP.Var 1406 "err"
  , WP.Seq 1406 1407
  , WP.Assign 1407 "stack_pointer" (WP.Num 0)
  , WP.Seq 1407 1408
  , WP.Branch 1408 (WP.Eq (WP.Plus (WP.Id "err") (WP.Num 0)) (WP.Num 1)) 1410 1511
  , WP.Assign 1410 "undefed" (WP.Num 0)
  , WP.Seq 1410 1411
  , WP.Assign 1411 "_tmp_old_op" (WP.Num 0)
  , WP.Seq 1411 1412
  , WP.Branch 1412 (WP.Eq (WP.Plus (WP.Id "_tmp_old_op") (WP.Num 0)) (WP.Num 1)) 1414 1457
  , WP.Assign 1414 "undefed" (WP.Num 0)
  , WP.Seq 1414 1415
  , WP.Var 1415 "op"
  , WP.Seq 1415 1416
  , WP.Branch 1416 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 0)) (WP.Num 1)) 1418 1433
  , WP.Var 1418 "tracer"
  , WP.Seq 1418 1419
  , WP.Branch 1419 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 1421 1422
  , WP.Var 1421 "data"
  , WP.Seq 1421 1422
  , WP.Seq 1421 1423
  , WP.Var 1422 "NOP_1422"
  , WP.Seq 1422 1423
  , WP.Var 1423 "IF_ELSE_FOOTER"
  , WP.Branch 1424 (WP.Eq (WP.Num 0) (WP.Num 1)) 1425 1431
  , WP.Var 1425 "tracer"
  , WP.Seq 1425 1426
  , WP.Branch 1426 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 1428 1429
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
  , WP.Branch 1435 (WP.Eq (WP.Num 0) (WP.Num 1)) 1436 1456
  , WP.Var 1436 "op"
  , WP.Seq 1436 1437
  , WP.Branch 1437 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 0)) (WP.Num 1)) 1439 1454
  , WP.Var 1439 "tracer"
  , WP.Seq 1439 1440
  , WP.Branch 1440 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 1442 1443
  , WP.Var 1442 "data"
  , WP.Seq 1442 1443
  , WP.Seq 1442 1444
  , WP.Var 1443 "NOP_1443"
  , WP.Seq 1443 1444
  , WP.Var 1444 "IF_ELSE_FOOTER"
  , WP.Branch 1445 (WP.Eq (WP.Num 0) (WP.Num 1)) 1446 1452
  , WP.Var 1446 "tracer"
  , WP.Seq 1446 1447
  , WP.Branch 1447 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 1449 1450
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
  , WP.Branch 1459 (WP.Eq (WP.Num 0) (WP.Num 1)) 1460 1509
  , WP.Assign 1460 "undefed" (WP.Num 0)
  , WP.Seq 1460 1461
  , WP.Assign 1461 "_tmp_old_op" (WP.Num 0)
  , WP.Seq 1461 1462
  , WP.Branch 1462 (WP.Eq (WP.Plus (WP.Id "_tmp_old_op") (WP.Num 0)) (WP.Num 1)) 1464 1507
  , WP.Assign 1464 "undefed" (WP.Num 0)
  , WP.Seq 1464 1465
  , WP.Var 1465 "op"
  , WP.Seq 1465 1466
  , WP.Branch 1466 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 0)) (WP.Num 1)) 1468 1483
  , WP.Var 1468 "tracer"
  , WP.Seq 1468 1469
  , WP.Branch 1469 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 1471 1472
  , WP.Var 1471 "data"
  , WP.Seq 1471 1472
  , WP.Seq 1471 1473
  , WP.Var 1472 "NOP_1472"
  , WP.Seq 1472 1473
  , WP.Var 1473 "IF_ELSE_FOOTER"
  , WP.Branch 1474 (WP.Eq (WP.Num 0) (WP.Num 1)) 1475 1481
  , WP.Var 1475 "tracer"
  , WP.Seq 1475 1476
  , WP.Branch 1476 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 1478 1479
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
  , WP.Branch 1485 (WP.Eq (WP.Num 0) (WP.Num 1)) 1486 1506
  , WP.Var 1486 "op"
  , WP.Seq 1486 1487
  , WP.Branch 1487 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 0)) (WP.Num 1)) 1489 1504
  , WP.Var 1489 "tracer"
  , WP.Seq 1489 1490
  , WP.Branch 1490 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 1492 1493
  , WP.Var 1492 "data"
  , WP.Seq 1492 1493
  , WP.Seq 1492 1494
  , WP.Var 1493 "NOP_1493"
  , WP.Seq 1493 1494
  , WP.Var 1494 "IF_ELSE_FOOTER"
  , WP.Branch 1495 (WP.Eq (WP.Num 0) (WP.Num 1)) 1496 1502
  , WP.Var 1496 "tracer"
  , WP.Seq 1496 1497
  , WP.Branch 1497 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 1499 1500
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
  , WP.Assign 1510 "stack_pointer" (WP.Num 0)
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
  , WP.Branch 1518 (WP.Eq (WP.Plus (WP.Plus (WP.Id "_i") (WP.Num 0)) (WP.Num 0)) (WP.Num 1)) 1519 1521
  , WP.Assign 1519 "tmp" (WP.Num 0)
  , WP.Seq 1519 1520
  , WP.Assign 1520 "undefed" (WP.Num 0)
  , WP.Seq 1520 1521
  , WP.Seq 1520 1518
  , WP.Var 1521 "LOOP_FOOTER"
  , WP.Seq 1521 1522
  , WP.Assign 1522 "tmp" (WP.Num 0)
  , WP.Seq 1522 1523
  , WP.Assign 1523 "self_or_null" (WP.Num 0)
  , WP.Seq 1523 1524
  , WP.Assign 1524 "undefed" (WP.Num 0)
  , WP.Seq 1524 1525
  , WP.Assign 1525 "tmp" (WP.Num 0)
  , WP.Seq 1525 1526
  , WP.Assign 1526 "callable" (WP.Num 0)
  , WP.Seq 1526 1527
  , WP.Assign 1527 "undefed" (WP.Num 0)
  , WP.Seq 1527 1528
  , WP.Assign 1528 "stack_pointer" (WP.Num 0)
  , WP.Seq 1528 1529
  , WP.Assign 1529 "stack_pointer" (WP.Num 0)
  , WP.Seq 1529 1530
  , WP.Branch 1530 (WP.Eq (WP.Plus (WP.Id "res_o") (WP.Num 0)) (WP.Num 1)) 1532 1532
  , WP.Seq 1531 1817
  , WP.Seq 1531 1533
  , WP.Var 1532 "NOP_1532"
  , WP.Seq 1532 1533
  , WP.Var 1533 "IF_ELSE_FOOTER"
  , WP.Assign 1534 "res" (WP.Num 0)
  , WP.Seq 1534 1535
  , WP.Assign 1535 "undefed" (WP.Num 0)
  , WP.Seq 1535 1536
  , WP.Assign 1536 "stack_pointer" (WP.Num 0)
  , WP.Seq 1536 1537
  , WP.Var 1537 "err"
  , WP.Seq 1537 1538
  , WP.Assign 1538 "stack_pointer" (WP.Num 0)
  , WP.Seq 1538 1539
  , WP.Branch 1539 (WP.Eq (WP.Plus (WP.Id "err") (WP.Num 0)) (WP.Num 1)) 1541 1541
  , WP.Seq 1540 1817
  , WP.Seq 1540 1542
  , WP.Var 1541 "NOP_1541"
  , WP.Seq 1541 1542
  , WP.Var 1542 "IF_ELSE_FOOTER"
  , WP.Var 1543 "word"
  , WP.Seq 1543 1544
  , WP.Assign 1544 "opcode" (WP.Num 0)
  , WP.Seq 1544 1545
  , WP.Assign 1545 "oparg" (WP.Num 0)
  , WP.Seq 1545 1546
  , WP.Branch 1546 (WP.Eq (WP.Num 0) (WP.Num 1)) 1547 1550
  , WP.Var 1547 "word"
  , WP.Seq 1547 1548
  , WP.Assign 1548 "opcode" (WP.Num 0)
  , WP.Seq 1548 1549
  , WP.Assign 1549 "oparg" (WP.Num 0)
  , WP.Seq 1549 1550
  , WP.Seq 1549 1546
  , WP.Var 1550 "LOOP_FOOTER"
  , WP.Seq 1550 1551
  , WP.Seq 1550 35
  , WP.Branch 1551 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 1553 1637
  , WP.Var 1553 "NOP_1553"
  , WP.Var 1554 "__CLABEL_TARGET_CALL_ALLOC_AND_ENTER_INIT"
  , WP.Seq 1554 1555
  , WP.Var 1555 "this_instr"
  , WP.Seq 1555 1556
  , WP.Assign 1556 "undefed" (WP.Num 0)
  , WP.Seq 1556 1557
  , WP.Assign 1557 "next_instr" (WP.Num 0)
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
  , WP.Branch 1565 (WP.Eq (WP.Num 0) (WP.Num 1)) 1567 1567
  , WP.Seq 1566 1310
  , WP.Seq 1566 1568
  , WP.Var 1567 "NOP_1567"
  , WP.Seq 1567 1568
  , WP.Var 1568 "IF_ELSE_FOOTER"
  , WP.Assign 1569 "self_or_null" (WP.Num 0)
  , WP.Seq 1569 1570
  , WP.Assign 1570 "callable" (WP.Num 0)
  , WP.Seq 1570 1571
  , WP.Var 1571 "type_version"
  , WP.Seq 1571 1572
  , WP.Var 1572 "callable_o"
  , WP.Seq 1572 1573
  , WP.Branch 1573 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 0)) (WP.Num 1)) 1575 1575
  , WP.Seq 1574 1310
  , WP.Seq 1574 1576
  , WP.Var 1575 "NOP_1575"
  , WP.Seq 1575 1576
  , WP.Var 1576 "IF_ELSE_FOOTER"
  , WP.Branch 1577 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 1579 1579
  , WP.Seq 1578 1310
  , WP.Seq 1578 1580
  , WP.Var 1579 "NOP_1579"
  , WP.Seq 1579 1580
  , WP.Var 1580 "IF_ELSE_FOOTER"
  , WP.Var 1581 "tp"
  , WP.Seq 1581 1582
  , WP.Branch 1582 (WP.Eq (WP.Plus (WP.Num 0) (WP.Id "type_version")) (WP.Num 1)) 1584 1584
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
  , WP.Branch 1589 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 1591 1591
  , WP.Seq 1590 1310
  , WP.Seq 1590 1592
  , WP.Var 1591 "NOP_1591"
  , WP.Seq 1591 1592
  , WP.Var 1592 "IF_ELSE_FOOTER"
  , WP.Var 1593 "self_o"
  , WP.Seq 1593 1594
  , WP.Assign 1594 "stack_pointer" (WP.Num 0)
  , WP.Seq 1594 1595
  , WP.Branch 1595 (WP.Eq (WP.Plus (WP.Id "self_o") (WP.Num 0)) (WP.Num 1)) 1597 1597
  , WP.Seq 1596 1817
  , WP.Seq 1596 1598
  , WP.Var 1597 "NOP_1597"
  , WP.Seq 1597 1598
  , WP.Var 1598 "IF_ELSE_FOOTER"
  , WP.Assign 1599 "self_or_null" (WP.Num 0)
  , WP.Seq 1599 1600
  , WP.Var 1600 "temp"
  , WP.Seq 1600 1601
  , WP.Assign 1601 "callable" (WP.Num 0)
  , WP.Seq 1601 1602
  , WP.Assign 1602 "undefed" (WP.Num 0)
  , WP.Seq 1602 1603
  , WP.Assign 1603 "undefed" (WP.Num 0)
  , WP.Seq 1603 1604
  , WP.Assign 1604 "stack_pointer" (WP.Num 0)
  , WP.Seq 1604 1605
  , WP.Assign 1605 "args" (WP.Num 0)
  , WP.Seq 1605 1606
  , WP.Assign 1606 "self" (WP.Num 0)
  , WP.Seq 1606 1607
  , WP.Assign 1607 "init" (WP.Num 0)
  , WP.Seq 1607 1608
  , WP.Var 1608 "shim"
  , WP.Seq 1608 1609
  , WP.Assign 1609 "stack_pointer" (WP.Num 0)
  , WP.Seq 1609 1610
  , WP.Assign 1610 "undefed" (WP.Num 0)
  , WP.Seq 1610 1611
  , WP.Var 1611 "temp"
  , WP.Seq 1611 1612
  , WP.Assign 1612 "stack_pointer" (WP.Num 0)
  , WP.Seq 1612 1613
  , WP.Assign 1613 "stack_pointer" (WP.Num 0)
  , WP.Seq 1613 1614
  , WP.Branch 1614 (WP.Eq (WP.Plus (WP.Id "temp") (WP.Num 0)) (WP.Num 1)) 1616 1617
  , WP.Assign 1616 "stack_pointer" (WP.Num 0)
  , WP.Seq 1616 1617
  , WP.Seq 1616 1817
  , WP.Seq 1616 1618
  , WP.Var 1617 "NOP_1617"
  , WP.Seq 1617 1618
  , WP.Var 1618 "IF_ELSE_FOOTER"
  , WP.Assign 1619 "undefed" (WP.Num 0)
  , WP.Seq 1619 1620
  , WP.Assign 1620 "init_frame" (WP.Num 0)
  , WP.Seq 1620 1621
  , WP.Assign 1621 "new_frame" (WP.Num 0)
  , WP.Seq 1621 1622
  , WP.Var 1622 "temp"
  , WP.Seq 1622 1623
  , WP.Assign 1623 "frame" (WP.Num 0)
  , WP.Seq 1623 1624
  , WP.Assign 1624 "stack_pointer" (WP.Num 0)
  , WP.Seq 1624 1625
  , WP.Assign 1625 "next_instr" (WP.Num 0)
  , WP.Seq 1625 1626
  , WP.Branch 1626 (WP.Eq (WP.Num 0) (WP.Num 1)) 1627 1628
  , WP.Assign 1627 "next_instr" (WP.Num 0)
  , WP.Seq 1627 1626
  , WP.Var 1628 "LOOP_FOOTER"
  , WP.Seq 1628 1629
  , WP.Var 1629 "word"
  , WP.Seq 1629 1630
  , WP.Assign 1630 "opcode" (WP.Num 0)
  , WP.Seq 1630 1631
  , WP.Assign 1631 "oparg" (WP.Num 0)
  , WP.Seq 1631 1632
  , WP.Branch 1632 (WP.Eq (WP.Num 0) (WP.Num 1)) 1633 1636
  , WP.Var 1633 "word"
  , WP.Seq 1633 1634
  , WP.Assign 1634 "opcode" (WP.Num 0)
  , WP.Seq 1634 1635
  , WP.Assign 1635 "oparg" (WP.Num 0)
  , WP.Seq 1635 1636
  , WP.Seq 1635 1632
  , WP.Var 1636 "LOOP_FOOTER"
  , WP.Seq 1636 1637
  , WP.Seq 1636 35
  , WP.Branch 1637 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 1639 1727
  , WP.Var 1639 "NOP_1639"
  , WP.Var 1640 "__CLABEL_TARGET_CALL_BOUND_METHOD_EXACT_ARGS"
  , WP.Seq 1640 1641
  , WP.Var 1641 "this_instr"
  , WP.Seq 1641 1642
  , WP.Assign 1642 "undefed" (WP.Num 0)
  , WP.Seq 1642 1643
  , WP.Assign 1643 "next_instr" (WP.Num 0)
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
  , WP.Branch 1649 (WP.Eq (WP.Num 0) (WP.Num 1)) 1651 1651
  , WP.Seq 1650 1310
  , WP.Seq 1650 1652
  , WP.Var 1651 "NOP_1651"
  , WP.Seq 1651 1652
  , WP.Var 1652 "IF_ELSE_FOOTER"
  , WP.Assign 1653 "null" (WP.Num 0)
  , WP.Seq 1653 1654
  , WP.Assign 1654 "callable" (WP.Num 0)
  , WP.Seq 1654 1655
  , WP.Branch 1655 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 0)) (WP.Num 1)) 1657 1657
  , WP.Seq 1656 1310
  , WP.Seq 1656 1658
  , WP.Var 1657 "NOP_1657"
  , WP.Seq 1657 1658
  , WP.Var 1658 "IF_ELSE_FOOTER"
  , WP.Branch 1659 (WP.Eq (WP.Plus (WP.Num 0) (WP.Plus (WP.Id "PyMethod_Type") (WP.Num 0))) (WP.Num 1)) 1661 1661
  , WP.Seq 1660 1310
  , WP.Seq 1660 1662
  , WP.Var 1661 "NOP_1661"
  , WP.Seq 1661 1662
  , WP.Var 1662 "IF_ELSE_FOOTER"
  , WP.Assign 1663 "self_or_null" (WP.Num 0)
  , WP.Seq 1663 1664
  , WP.Var 1664 "callable_o"
  , WP.Seq 1664 1665
  , WP.Assign 1665 "self_or_null" (WP.Num 0)
  , WP.Seq 1665 1666
  , WP.Var 1666 "temp"
  , WP.Seq 1666 1667
  , WP.Assign 1667 "callable" (WP.Num 0)
  , WP.Seq 1667 1668
  , WP.Assign 1668 "undefed" (WP.Num 0)
  , WP.Seq 1668 1669
  , WP.Assign 1669 "undefed" (WP.Num 0)
  , WP.Seq 1669 1670
  , WP.Assign 1670 "stack_pointer" (WP.Num 0)
  , WP.Seq 1670 1671
  , WP.Var 1671 "func_version"
  , WP.Seq 1671 1672
  , WP.Var 1672 "callable_o"
  , WP.Seq 1672 1673
  , WP.Branch 1673 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Plus (WP.Id "PyFunction_Type") (WP.Num 0))) (WP.Num 0)) (WP.Num 1)) 1675 1675
  , WP.Seq 1674 1310
  , WP.Seq 1674 1676
  , WP.Var 1675 "NOP_1675"
  , WP.Seq 1675 1676
  , WP.Var 1676 "IF_ELSE_FOOTER"
  , WP.Var 1677 "func"
  , WP.Seq 1677 1678
  , WP.Branch 1678 (WP.Eq (WP.Plus (WP.Num 0) (WP.Id "func_version")) (WP.Num 1)) 1680 1680
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
  , WP.Branch 1685 (WP.Eq (WP.Plus (WP.Num 0) (WP.Plus (WP.Id "oparg") (WP.Plus (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 0)))) (WP.Num 1)) 1687 1687
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
  , WP.Branch 1692 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 1694 1694
  , WP.Seq 1693 1310
  , WP.Seq 1693 1695
  , WP.Var 1694 "NOP_1694"
  , WP.Seq 1694 1695
  , WP.Var 1695 "IF_ELSE_FOOTER"
  , WP.Branch 1696 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 1698 1698
  , WP.Seq 1697 1310
  , WP.Seq 1697 1699
  , WP.Var 1698 "NOP_1698"
  , WP.Seq 1698 1699
  , WP.Var 1699 "IF_ELSE_FOOTER"
  , WP.Assign 1700 "args" (WP.Num 0)
  , WP.Seq 1700 1701
  , WP.Var 1701 "has_self"
  , WP.Seq 1701 1702
  , WP.Var 1702 "pushed_frame"
  , WP.Seq 1702 1703
  , WP.Var 1703 "first_non_self_local"
  , WP.Seq 1703 1704
  , WP.Assign 1704 "undefed" (WP.Num 0)
  , WP.Seq 1704 1705
  , WP.Var 1705 "i"
  , WP.Seq 1705 1706
  , WP.Branch 1706 (WP.Eq (WP.Plus (WP.Id "i") (WP.Id "oparg")) (WP.Num 1)) 1707 1708
  , WP.Assign 1707 "undefed" (WP.Num 0)
  , WP.Seq 1707 1708
  , WP.Seq 1707 1706
  , WP.Var 1708 "LOOP_FOOTER"
  , WP.Seq 1708 1709
  , WP.Assign 1709 "new_frame" (WP.Num 0)
  , WP.Seq 1709 1710
  , WP.Assign 1710 "undefed" (WP.Num 0)
  , WP.Seq 1710 1711
  , WP.Var 1711 "temp"
  , WP.Seq 1711 1712
  , WP.Assign 1712 "stack_pointer" (WP.Num 0)
  , WP.Seq 1712 1713
  , WP.Assign 1713 "frame" (WP.Num 0)
  , WP.Seq 1713 1714
  , WP.Assign 1714 "stack_pointer" (WP.Num 0)
  , WP.Seq 1714 1715
  , WP.Assign 1715 "next_instr" (WP.Num 0)
  , WP.Seq 1715 1716
  , WP.Branch 1716 (WP.Eq (WP.Num 0) (WP.Num 1)) 1717 1718
  , WP.Assign 1717 "next_instr" (WP.Num 0)
  , WP.Seq 1717 1716
  , WP.Var 1718 "LOOP_FOOTER"
  , WP.Seq 1718 1719
  , WP.Var 1719 "word"
  , WP.Seq 1719 1720
  , WP.Assign 1720 "opcode" (WP.Num 0)
  , WP.Seq 1720 1721
  , WP.Assign 1721 "oparg" (WP.Num 0)
  , WP.Seq 1721 1722
  , WP.Branch 1722 (WP.Eq (WP.Num 0) (WP.Num 1)) 1723 1726
  , WP.Var 1723 "word"
  , WP.Seq 1723 1724
  , WP.Assign 1724 "opcode" (WP.Num 0)
  , WP.Seq 1724 1725
  , WP.Assign 1725 "oparg" (WP.Num 0)
  , WP.Seq 1725 1726
  , WP.Seq 1725 1722
  , WP.Var 1726 "LOOP_FOOTER"
  , WP.Seq 1726 1727
  , WP.Seq 1726 35
  , WP.Branch 1727 (WP.Eq (WP.Plus (WP.Id "opcode") (WP.Num 0)) (WP.Num 1)) 1729 1810
  , WP.Var 1729 "NOP_1729"
  , WP.Var 1730 "__CLABEL_TARGET_CALL_BOUND_METHOD_GENERAL"
  , WP.Seq 1730 1731
  , WP.Var 1731 "this_instr"
  , WP.Seq 1731 1732
  , WP.Assign 1732 "undefed" (WP.Num 0)
  , WP.Seq 1732 1733
  , WP.Assign 1733 "next_instr" (WP.Num 0)
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
  , WP.Branch 1739 (WP.Eq (WP.Num 0) (WP.Num 1)) 1741 1741
  , WP.Seq 1740 1310
  , WP.Seq 1740 1742
  , WP.Var 1741 "NOP_1741"
  , WP.Seq 1741 1742
  , WP.Var 1742 "IF_ELSE_FOOTER"
  , WP.Assign 1743 "null" (WP.Num 0)
  , WP.Seq 1743 1744
  , WP.Assign 1744 "callable" (WP.Num 0)
  , WP.Seq 1744 1745
  , WP.Var 1745 "func_version"
  , WP.Seq 1745 1746
  , WP.Var 1746 "callable_o"
  , WP.Seq 1746 1747
  , WP.Branch 1747 (WP.Eq (WP.Plus (WP.Num 0) (WP.Plus (WP.Id "PyMethod_Type") (WP.Num 0))) (WP.Num 1)) 1749 1749
  , WP.Seq 1748 1310
  , WP.Seq 1748 1750
  , WP.Var 1749 "NOP_1749"
  , WP.Seq 1749 1750
  , WP.Var 1750 "IF_ELSE_FOOTER"
  , WP.Var 1751 "func"
  , WP.Seq 1751 1752
  , WP.Branch 1752 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Plus (WP.Id "PyFunction_Type") (WP.Num 0))) (WP.Num 0)) (WP.Num 1)) 1754 1754
  , WP.Seq 1753 1310
  , WP.Seq 1753 1755
  , WP.Var 1754 "NOP_1754"
  , WP.Seq 1754 1755
  , WP.Var 1755 "IF_ELSE_FOOTER"
  , WP.Branch 1756 (WP.Eq (WP.Plus (WP.Num 0) (WP.Id "func_version")) (WP.Num 1)) 1758 1758
  , WP.Seq 1757 1310
  , WP.Seq 1757 1759
  , WP.Var 1758 "NOP_1758"
  , WP.Seq 1758 1759
  , WP.Var 1759 "IF_ELSE_FOOTER"
  , WP.Branch 1760 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 0)) (WP.Num 1)) 1762 1762
  , WP.Seq 1761 1310
  , WP.Seq 1761 1763
  , WP.Var 1762 "NOP_1762"
  , WP.Seq 1762 1763
  , WP.Var 1763 "IF_ELSE_FOOTER"
  , WP.Assign 1764 "self_or_null" (WP.Num 0)
  , WP.Seq 1764 1765
  , WP.Var 1765 "callable_o"
  , WP.Seq 1765 1766
  , WP.Assign 1766 "self_or_null" (WP.Num 0)
  , WP.Seq 1766 1767
  , WP.Var 1767 "temp"
  , WP.Seq 1767 1768
  , WP.Assign 1768 "callable" (WP.Num 0)
  , WP.Seq 1768 1769
  , WP.Assign 1769 "undefed" (WP.Num 0)
  , WP.Seq 1769 1770
  , WP.Assign 1770 "undefed" (WP.Num 0)
  , WP.Seq 1770 1771
  , WP.Assign 1771 "stack_pointer" (WP.Num 0)
  , WP.Seq 1771 1772
  , WP.Branch 1772 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 1774 1774
  , WP.Seq 1773 1310
  , WP.Seq 1773 1775
  , WP.Var 1774 "NOP_1774"
  , WP.Seq 1774 1775
  , WP.Var 1775 "IF_ELSE_FOOTER"
  , WP.Assign 1776 "args" (WP.Num 0)
  , WP.Seq 1776 1777
  , WP.Var 1777 "callable_o"
  , WP.Seq 1777 1778
  , WP.Var 1778 "total_args"
  , WP.Seq 1778 1779
  , WP.Branch 1779 (WP.Eq (WP.Plus (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 0)) (WP.Num 1)) 1781 1782
  , WP.Assign 1781 "total_args" (WP.Num 0)
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
  , WP.Assign 1787 "stack_pointer" (WP.Num 0)
  , WP.Seq 1787 1788
  , WP.Assign 1788 "stack_pointer" (WP.Num 0)
  , WP.Seq 1788 1789
  , WP.Branch 1789 (WP.Eq (WP.Plus (WP.Id "temp") (WP.Num 0)) (WP.Num 1)) 1791 1791
  , WP.Seq 1790 1817
  , WP.Seq 1790 1792
  , WP.Var 1791 "NOP_1791"
  , WP.Seq 1791 1792
  , WP.Var 1792 "IF_ELSE_FOOTER"
  , WP.Assign 1793 "new_frame" (WP.Num 0)
  , WP.Seq 1793 1794
  , WP.Assign 1794 "undefed" (WP.Num 0)
  , WP.Seq 1794 1795
  , WP.Var 1795 "temp"
  , WP.Seq 1795 1796
  , WP.Assign 1796 "frame" (WP.Num 0)
  , WP.Seq 1796 1797
  , WP.Assign 1797 "stack_pointer" (WP.Num 0)
  , WP.Seq 1797 1798
  , WP.Assign 1798 "next_instr" (WP.Num 0)
  , WP.Seq 1798 1799
  , WP.Branch 1799 (WP.Eq (WP.Num 0) (WP.Num 1)) 1800 1801
  , WP.Assign 1800 "next_instr" (WP.Num 0)
  , WP.Seq 1800 1799
  , WP.Var 1801 "LOOP_FOOTER"
  , WP.Seq 1801 1802
  , WP.Var 1802 "word"
  , WP.Seq 1802 1803
  , WP.Assign 1803 "opcode" (WP.Num 0)
  , WP.Seq 1803 1804
  , WP.Assign 1804 "oparg" (WP.Num 0)
  , WP.Seq 1804 1805
  , WP.Branch 1805 (WP.Eq (WP.Num 0) (WP.Num 1)) 1806 1809
  , WP.Var 1806 "word"
  , WP.Seq 1806 1807
  , WP.Assign 1807 "opcode" (WP.Num 0)
  , WP.Seq 1807 1808
  , WP.Assign 1808 "oparg" (WP.Num 0)
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
  , WP.Assign 1814 "stack_pointer" (WP.Num 0)
  , WP.Seq 1814 1815
  , WP.Seq 1814 1817
  , WP.Var 1815 "__CLABEL_pop_1_error"
  , WP.Seq 1815 1816
  , WP.Assign 1816 "stack_pointer" (WP.Num 0)
  , WP.Seq 1816 1817
  , WP.Seq 1816 1817
  , WP.Var 1817 "__CLABEL_error"
  , WP.Seq 1817 1818
  , WP.Branch 1818 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 1820 1821
  , WP.Assign 1820 "stack_pointer" (WP.Num 0)
  , WP.Seq 1820 1822
  , WP.Var 1821 "NOP_1821"
  , WP.Seq 1821 1822
  , WP.Var 1822 "IF_ELSE_FOOTER"
  , WP.Branch 1823 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 1825 1832
  , WP.Var 1825 "f"
  , WP.Seq 1825 1826
  , WP.Assign 1826 "stack_pointer" (WP.Num 0)
  , WP.Seq 1826 1827
  , WP.Branch 1827 (WP.Eq (WP.Plus (WP.Id "f") (WP.Num 0)) (WP.Num 1)) 1829 1830
  , WP.Assign 1829 "stack_pointer" (WP.Num 0)
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
  , WP.Branch 1840 (WP.Eq (WP.Plus (WP.Id "handled") (WP.Num 0)) (WP.Num 1)) 1842 1846
  , WP.Var 1842 "stackbase"
  , WP.Seq 1842 1843
  , WP.Branch 1843 (WP.Eq (WP.Plus (WP.Num 0) (WP.Id "stackbase")) (WP.Num 1)) 1844 1845
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
  , WP.Branch 1849 (WP.Eq (WP.Plus (WP.Num 0) (WP.Id "new_top")) (WP.Num 1)) 1850 1851
  , WP.Var 1850 "ref"
  , WP.Seq 1850 1851
  , WP.Seq 1850 1849
  , WP.Var 1851 "LOOP_FOOTER"
  , WP.Seq 1851 1852
  , WP.Branch 1852 (WP.Eq (WP.Id "lasti") (WP.Num 1)) 1854 1856
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
  , WP.Assign 1859 "next_instr" (WP.Num 0)
  , WP.Seq 1859 1860
  , WP.Var 1860 "err"
  , WP.Seq 1860 1861
  , WP.Branch 1861 (WP.Eq (WP.Plus (WP.Id "err") (WP.Num 0)) (WP.Num 1)) 1863 1863
  , WP.Seq 1862 1834
  , WP.Seq 1862 1864
  , WP.Var 1863 "NOP_1863"
  , WP.Seq 1863 1864
  , WP.Var 1864 "IF_ELSE_FOOTER"
  , WP.Assign 1865 "stack_pointer" (WP.Num 0)
  , WP.Seq 1865 1866
  , WP.Var 1866 "word"
  , WP.Seq 1866 1867
  , WP.Assign 1867 "opcode" (WP.Num 0)
  , WP.Seq 1867 1868
  , WP.Assign 1868 "oparg" (WP.Num 0)
  , WP.Seq 1868 1869
  , WP.Branch 1869 (WP.Eq (WP.Num 0) (WP.Num 1)) 1870 1873
  , WP.Var 1870 "word"
  , WP.Seq 1870 1871
  , WP.Assign 1871 "opcode" (WP.Num 0)
  , WP.Seq 1871 1872
  , WP.Assign 1872 "oparg" (WP.Num 0)
  , WP.Seq 1872 1873
  , WP.Seq 1872 1869
  , WP.Var 1873 "LOOP_FOOTER"
  , WP.Seq 1873 1874
  , WP.Seq 1873 35
  , WP.Var 1874 "__CLABEL_exit_unwind"
  , WP.Seq 1874 1875
  , WP.Var 1875 "dying"
  , WP.Seq 1875 1876
  , WP.Assign 1876 "frame" (WP.Num 0)
  , WP.Seq 1876 1877
  , WP.Assign 1877 "undefed" (WP.Num 0)
  , WP.Seq 1877 1878
  , WP.Branch 1878 (WP.Eq (WP.Plus (WP.Num 0) (WP.Id "FRAME_OWNED_BY_INTERPRETER")) (WP.Num 1)) 1880 1882
  , WP.Assign 1880 "undefed" (WP.Num 0)
  , WP.Seq 1880 1881
  , WP.Assign 1881 "return" (WP.Num 0)
  , WP.Seq 1881 1882
  , WP.Seq 1881 1883
  , WP.Var 1882 "NOP_1882"
  , WP.Seq 1882 1883
  , WP.Var 1883 "IF_ELSE_FOOTER"
  , WP.Assign 1884 "next_instr" (WP.Num 0)
  , WP.Seq 1884 1885
  , WP.Assign 1885 "stack_pointer" (WP.Num 0)
  , WP.Seq 1885 1886
  , WP.Seq 1885 1817
  , WP.Var 1886 "__CLABEL_start_frame"
  , WP.Seq 1886 1887
  , WP.Var 1887 "too_deep"
  , WP.Seq 1887 1888
  , WP.Branch 1888 (WP.Eq (WP.Id "too_deep") (WP.Num 1)) 1890 1890
  , WP.Seq 1889 1874
  , WP.Seq 1889 1891
  , WP.Var 1890 "NOP_1890"
  , WP.Seq 1890 1891
  , WP.Var 1891 "IF_ELSE_FOOTER"
  , WP.Assign 1892 "next_instr" (WP.Num 0)
  , WP.Seq 1892 1893
  , WP.Assign 1893 "stack_pointer" (WP.Num 0)
  , WP.Seq 1893 1894
  , WP.Var 1894 "word"
  , WP.Seq 1894 1895
  , WP.Assign 1895 "opcode" (WP.Num 0)
  , WP.Seq 1895 1896
  , WP.Assign 1896 "oparg" (WP.Num 0)
  , WP.Seq 1896 1897
  , WP.Branch 1897 (WP.Eq (WP.Num 0) (WP.Num 1)) 1898 1901
  , WP.Var 1898 "word"
  , WP.Seq 1898 1899
  , WP.Assign 1899 "opcode" (WP.Num 0)
  , WP.Seq 1899 1900
  , WP.Assign 1900 "oparg" (WP.Num 0)
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
  , WP.Assign 1905 "frame" (WP.Num 0)
  , WP.Seq 1905 1906
  , WP.Assign 1906 "undefed" (WP.Num 0)
  , WP.Seq 1906 1907
  , WP.Assign 1907 "undefed" (WP.Num 0)
  , WP.Seq 1907 1908
  , WP.Assign 1908 "return" (WP.Num 0)
  , WP.Seq 1908 1909
  , WP.Seq 1908 1909
  , WP.Var 1909 "PROG_END"
  ]
