module ReducedProduct.CEval1K where

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
  , H.mkSeq 28 899
  , H.mkSeq 28 30
  , H.mkVar 29 "NOP_29"
  , H.mkSeq 29 30
  , H.mkVar 30 "IF_ELSE_FOOTER"
  , H.mkAssign 31 "next_instr" (H.Num 0)
  , H.mkSeq 31 32
  , H.mkAssign 32 "stack_pointer" (H.Num 0)
  , H.mkSeq 32 33
  , H.mkSeq 32 814
  , H.mkSeq 32 34
  , H.mkVar 33 "NOP_33"
  , H.mkSeq 33 34
  , H.mkVar 34 "IF_ELSE_FOOTER"
  , H.mkSeq 34 883
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
  , H.mkSeq 67 814
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
  , H.mkSeq 116 810
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
  , H.mkSeq 200 810
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
  , H.mkSeq 342 814
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
  , H.mkSeq 383 810
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
  , H.mkSeq 475 814
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
  , H.mkSeq 643 814
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
  , H.mkSeq 794 810
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
  , H.mkVar 807 "NOP_807"
  , H.mkSeq 807 808
  , H.mkVar 808 "__CLABEL_CODEGEN_SWITCH_EXIT_0"
  , H.mkSeq 808 809
  , H.mkVar 809 "NOP_809"
  , H.mkVar 810 "__CLABEL_pop_2_error"
  , H.mkSeq 810 811
  , H.mkAssign 811 "stack_pointer" (H.Num 0)
  , H.mkSeq 811 812
  , H.mkSeq 811 814
  , H.mkVar 812 "__CLABEL_pop_1_error"
  , H.mkSeq 812 813
  , H.mkAssign 813 "stack_pointer" (H.Num 0)
  , H.mkSeq 813 814
  , H.mkSeq 813 814
  , H.mkVar 814 "__CLABEL_error"
  , H.mkSeq 814 815
  , H.mkBranch 815 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 817 818
  , H.mkAssign 817 "stack_pointer" (H.Num 0)
  , H.mkSeq 817 819
  , H.mkVar 818 "NOP_818"
  , H.mkSeq 818 819
  , H.mkVar 819 "IF_ELSE_FOOTER"
  , H.mkBranch 820 (H.Eq (H.Plus (H.Num 0) (H.Num 0)) (H.Num 1)) 822 829
  , H.mkVar 822 "f"
  , H.mkSeq 822 823
  , H.mkAssign 823 "stack_pointer" (H.Num 0)
  , H.mkSeq 823 824
  , H.mkBranch 824 (H.Eq (H.Plus (H.Id "f") (H.Num 0)) (H.Num 1)) 826 827
  , H.mkAssign 826 "stack_pointer" (H.Num 0)
  , H.mkSeq 826 828
  , H.mkVar 827 "NOP_827"
  , H.mkSeq 827 828
  , H.mkVar 828 "IF_ELSE_FOOTER"
  , H.mkSeq 828 830
  , H.mkVar 829 "NOP_829"
  , H.mkSeq 829 830
  , H.mkVar 830 "IF_ELSE_FOOTER"
  , H.mkSeq 830 831
  , H.mkVar 831 "__CLABEL_exception_unwind"
  , H.mkSeq 831 832
  , H.mkVar 832 "offset"
  , H.mkSeq 832 833
  , H.mkVar 833 "level"
  , H.mkSeq 833 834
  , H.mkVar 834 "handler"
  , H.mkSeq 834 835
  , H.mkVar 835 "lasti"
  , H.mkSeq 835 836
  , H.mkVar 836 "handled"
  , H.mkSeq 836 837
  , H.mkBranch 837 (H.Eq (H.Plus (H.Id "handled") (H.Num 0)) (H.Num 1)) 839 843
  , H.mkVar 839 "stackbase"
  , H.mkSeq 839 840
  , H.mkBranch 840 (H.Eq (H.Plus (H.Num 0) (H.Id "stackbase")) (H.Num 1)) 841 842
  , H.mkVar 841 "ref"
  , H.mkSeq 841 842
  , H.mkSeq 841 840
  , H.mkVar 842 "LOOP_FOOTER"
  , H.mkSeq 842 843
  , H.mkSeq 842 871
  , H.mkSeq 842 844
  , H.mkVar 843 "NOP_843"
  , H.mkSeq 843 844
  , H.mkVar 844 "IF_ELSE_FOOTER"
  , H.mkVar 845 "new_top"
  , H.mkSeq 845 846
  , H.mkBranch 846 (H.Eq (H.Plus (H.Num 0) (H.Id "new_top")) (H.Num 1)) 847 848
  , H.mkVar 847 "ref"
  , H.mkSeq 847 848
  , H.mkSeq 847 846
  , H.mkVar 848 "LOOP_FOOTER"
  , H.mkSeq 848 849
  , H.mkBranch 849 (H.Eq (H.Id "lasti") (H.Num 1)) 851 853
  , H.mkVar 851 "frame_lasti"
  , H.mkSeq 851 852
  , H.mkVar 852 "lasti"
  , H.mkSeq 852 853
  , H.mkSeq 852 854
  , H.mkVar 853 "NOP_853"
  , H.mkSeq 853 854
  , H.mkVar 854 "IF_ELSE_FOOTER"
  , H.mkVar 855 "exc"
  , H.mkSeq 855 856
  , H.mkAssign 856 "next_instr" (H.Num 0)
  , H.mkSeq 856 857
  , H.mkVar 857 "err"
  , H.mkSeq 857 858
  , H.mkBranch 858 (H.Eq (H.Plus (H.Id "err") (H.Num 0)) (H.Num 1)) 860 860
  , H.mkSeq 859 831
  , H.mkSeq 859 861
  , H.mkVar 860 "NOP_860"
  , H.mkSeq 860 861
  , H.mkVar 861 "IF_ELSE_FOOTER"
  , H.mkAssign 862 "stack_pointer" (H.Num 0)
  , H.mkSeq 862 863
  , H.mkVar 863 "word"
  , H.mkSeq 863 864
  , H.mkAssign 864 "opcode" (H.Num 0)
  , H.mkSeq 864 865
  , H.mkAssign 865 "oparg" (H.Num 0)
  , H.mkSeq 865 866
  , H.mkBranch 866 (H.Eq (H.Num 0) (H.Num 1)) 867 870
  , H.mkVar 867 "word"
  , H.mkSeq 867 868
  , H.mkAssign 868 "opcode" (H.Num 0)
  , H.mkSeq 868 869
  , H.mkAssign 869 "oparg" (H.Num 0)
  , H.mkSeq 869 870
  , H.mkSeq 869 866
  , H.mkVar 870 "LOOP_FOOTER"
  , H.mkSeq 870 871
  , H.mkSeq 870 35
  , H.mkVar 871 "__CLABEL_exit_unwind"
  , H.mkSeq 871 872
  , H.mkVar 872 "dying"
  , H.mkSeq 872 873
  , H.mkAssign 873 "frame" (H.Num 0)
  , H.mkSeq 873 874
  , H.mkAssign 874 "undefed" (H.Num 0)
  , H.mkSeq 874 875
  , H.mkBranch 875 (H.Eq (H.Plus (H.Num 0) (H.Id "FRAME_OWNED_BY_INTERPRETER")) (H.Num 1)) 877 879
  , H.mkAssign 877 "undefed" (H.Num 0)
  , H.mkSeq 877 878
  , H.mkAssign 878 "return" (H.Num 0)
  , H.mkSeq 878 879
  , H.mkSeq 878 880
  , H.mkVar 879 "NOP_879"
  , H.mkSeq 879 880
  , H.mkVar 880 "IF_ELSE_FOOTER"
  , H.mkAssign 881 "next_instr" (H.Num 0)
  , H.mkSeq 881 882
  , H.mkAssign 882 "stack_pointer" (H.Num 0)
  , H.mkSeq 882 883
  , H.mkSeq 882 814
  , H.mkVar 883 "__CLABEL_start_frame"
  , H.mkSeq 883 884
  , H.mkVar 884 "too_deep"
  , H.mkSeq 884 885
  , H.mkBranch 885 (H.Eq (H.Id "too_deep") (H.Num 1)) 887 887
  , H.mkSeq 886 871
  , H.mkSeq 886 888
  , H.mkVar 887 "NOP_887"
  , H.mkSeq 887 888
  , H.mkVar 888 "IF_ELSE_FOOTER"
  , H.mkAssign 889 "next_instr" (H.Num 0)
  , H.mkSeq 889 890
  , H.mkAssign 890 "stack_pointer" (H.Num 0)
  , H.mkSeq 890 891
  , H.mkVar 891 "word"
  , H.mkSeq 891 892
  , H.mkAssign 892 "opcode" (H.Num 0)
  , H.mkSeq 892 893
  , H.mkAssign 893 "oparg" (H.Num 0)
  , H.mkSeq 893 894
  , H.mkBranch 894 (H.Eq (H.Num 0) (H.Num 1)) 895 898
  , H.mkVar 895 "word"
  , H.mkSeq 895 896
  , H.mkAssign 896 "opcode" (H.Num 0)
  , H.mkSeq 896 897
  , H.mkAssign 897 "oparg" (H.Num 0)
  , H.mkSeq 897 898
  , H.mkSeq 897 894
  , H.mkVar 898 "LOOP_FOOTER"
  , H.mkSeq 898 899
  , H.mkSeq 898 35
  , H.mkVar 899 "__CLABEL_early_exit"
  , H.mkSeq 899 900
  , H.mkVar 900 "NOP_900"
  , H.mkVar 901 "dying"
  , H.mkSeq 901 902
  , H.mkAssign 902 "frame" (H.Num 0)
  , H.mkSeq 902 903
  , H.mkAssign 903 "undefed" (H.Num 0)
  , H.mkSeq 903 904
  , H.mkAssign 904 "undefed" (H.Num 0)
  , H.mkSeq 904 905
  , H.mkAssign 905 "return" (H.Num 0)
  , H.mkSeq 905 906
  , H.mkSeq 905 906
  , H.mkVar 906 "PROG_END"
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
  , NP.Seq 28 899
  , NP.Seq 28 30
  , NP.Var 29 "NOP_29"
  , NP.Seq 29 30
  , NP.Var 30 "IF_ELSE_FOOTER"
  , NP.Assign 31 "next_instr" (NP.Num 0)
  , NP.Seq 31 32
  , NP.Assign 32 "stack_pointer" (NP.Num 0)
  , NP.Seq 32 33
  , NP.Seq 32 814
  , NP.Seq 32 34
  , NP.Var 33 "NOP_33"
  , NP.Seq 33 34
  , NP.Var 34 "IF_ELSE_FOOTER"
  , NP.Seq 34 883
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
  , NP.Seq 67 814
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
  , NP.Seq 116 810
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
  , NP.Seq 200 810
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
  , NP.Seq 342 814
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
  , NP.Seq 383 810
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
  , NP.Seq 475 814
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
  , NP.Seq 643 814
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
  , NP.Seq 794 810
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
  , NP.Var 807 "NOP_807"
  , NP.Seq 807 808
  , NP.Var 808 "__CLABEL_CODEGEN_SWITCH_EXIT_0"
  , NP.Seq 808 809
  , NP.Var 809 "NOP_809"
  , NP.Var 810 "__CLABEL_pop_2_error"
  , NP.Seq 810 811
  , NP.Assign 811 "stack_pointer" (NP.Num 0)
  , NP.Seq 811 812
  , NP.Seq 811 814
  , NP.Var 812 "__CLABEL_pop_1_error"
  , NP.Seq 812 813
  , NP.Assign 813 "stack_pointer" (NP.Num 0)
  , NP.Seq 813 814
  , NP.Seq 813 814
  , NP.Var 814 "__CLABEL_error"
  , NP.Seq 814 815
  , NP.Branch 815 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 817 818
  , NP.Assign 817 "stack_pointer" (NP.Num 0)
  , NP.Seq 817 819
  , NP.Var 818 "NOP_818"
  , NP.Seq 818 819
  , NP.Var 819 "IF_ELSE_FOOTER"
  , NP.Branch 820 (NP.Eq (NP.Plus (NP.Num 0) (NP.Num 0)) (NP.Num 1)) 822 829
  , NP.Var 822 "f"
  , NP.Seq 822 823
  , NP.Assign 823 "stack_pointer" (NP.Num 0)
  , NP.Seq 823 824
  , NP.Branch 824 (NP.Eq (NP.Plus (NP.Id "f") (NP.Num 0)) (NP.Num 1)) 826 827
  , NP.Assign 826 "stack_pointer" (NP.Num 0)
  , NP.Seq 826 828
  , NP.Var 827 "NOP_827"
  , NP.Seq 827 828
  , NP.Var 828 "IF_ELSE_FOOTER"
  , NP.Seq 828 830
  , NP.Var 829 "NOP_829"
  , NP.Seq 829 830
  , NP.Var 830 "IF_ELSE_FOOTER"
  , NP.Seq 830 831
  , NP.Var 831 "__CLABEL_exception_unwind"
  , NP.Seq 831 832
  , NP.Var 832 "offset"
  , NP.Seq 832 833
  , NP.Var 833 "level"
  , NP.Seq 833 834
  , NP.Var 834 "handler"
  , NP.Seq 834 835
  , NP.Var 835 "lasti"
  , NP.Seq 835 836
  , NP.Var 836 "handled"
  , NP.Seq 836 837
  , NP.Branch 837 (NP.Eq (NP.Plus (NP.Id "handled") (NP.Num 0)) (NP.Num 1)) 839 843
  , NP.Var 839 "stackbase"
  , NP.Seq 839 840
  , NP.Branch 840 (NP.Eq (NP.Plus (NP.Num 0) (NP.Id "stackbase")) (NP.Num 1)) 841 842
  , NP.Var 841 "ref"
  , NP.Seq 841 842
  , NP.Seq 841 840
  , NP.Var 842 "LOOP_FOOTER"
  , NP.Seq 842 843
  , NP.Seq 842 871
  , NP.Seq 842 844
  , NP.Var 843 "NOP_843"
  , NP.Seq 843 844
  , NP.Var 844 "IF_ELSE_FOOTER"
  , NP.Var 845 "new_top"
  , NP.Seq 845 846
  , NP.Branch 846 (NP.Eq (NP.Plus (NP.Num 0) (NP.Id "new_top")) (NP.Num 1)) 847 848
  , NP.Var 847 "ref"
  , NP.Seq 847 848
  , NP.Seq 847 846
  , NP.Var 848 "LOOP_FOOTER"
  , NP.Seq 848 849
  , NP.Branch 849 (NP.Eq (NP.Id "lasti") (NP.Num 1)) 851 853
  , NP.Var 851 "frame_lasti"
  , NP.Seq 851 852
  , NP.Var 852 "lasti"
  , NP.Seq 852 853
  , NP.Seq 852 854
  , NP.Var 853 "NOP_853"
  , NP.Seq 853 854
  , NP.Var 854 "IF_ELSE_FOOTER"
  , NP.Var 855 "exc"
  , NP.Seq 855 856
  , NP.Assign 856 "next_instr" (NP.Num 0)
  , NP.Seq 856 857
  , NP.Var 857 "err"
  , NP.Seq 857 858
  , NP.Branch 858 (NP.Eq (NP.Plus (NP.Id "err") (NP.Num 0)) (NP.Num 1)) 860 860
  , NP.Seq 859 831
  , NP.Seq 859 861
  , NP.Var 860 "NOP_860"
  , NP.Seq 860 861
  , NP.Var 861 "IF_ELSE_FOOTER"
  , NP.Assign 862 "stack_pointer" (NP.Num 0)
  , NP.Seq 862 863
  , NP.Var 863 "word"
  , NP.Seq 863 864
  , NP.Assign 864 "opcode" (NP.Num 0)
  , NP.Seq 864 865
  , NP.Assign 865 "oparg" (NP.Num 0)
  , NP.Seq 865 866
  , NP.Branch 866 (NP.Eq (NP.Num 0) (NP.Num 1)) 867 870
  , NP.Var 867 "word"
  , NP.Seq 867 868
  , NP.Assign 868 "opcode" (NP.Num 0)
  , NP.Seq 868 869
  , NP.Assign 869 "oparg" (NP.Num 0)
  , NP.Seq 869 870
  , NP.Seq 869 866
  , NP.Var 870 "LOOP_FOOTER"
  , NP.Seq 870 871
  , NP.Seq 870 35
  , NP.Var 871 "__CLABEL_exit_unwind"
  , NP.Seq 871 872
  , NP.Var 872 "dying"
  , NP.Seq 872 873
  , NP.Assign 873 "frame" (NP.Num 0)
  , NP.Seq 873 874
  , NP.Assign 874 "undefed" (NP.Num 0)
  , NP.Seq 874 875
  , NP.Branch 875 (NP.Eq (NP.Plus (NP.Num 0) (NP.Id "FRAME_OWNED_BY_INTERPRETER")) (NP.Num 1)) 877 879
  , NP.Assign 877 "undefed" (NP.Num 0)
  , NP.Seq 877 878
  , NP.Assign 878 "return" (NP.Num 0)
  , NP.Seq 878 879
  , NP.Seq 878 880
  , NP.Var 879 "NOP_879"
  , NP.Seq 879 880
  , NP.Var 880 "IF_ELSE_FOOTER"
  , NP.Assign 881 "next_instr" (NP.Num 0)
  , NP.Seq 881 882
  , NP.Assign 882 "stack_pointer" (NP.Num 0)
  , NP.Seq 882 883
  , NP.Seq 882 814
  , NP.Var 883 "__CLABEL_start_frame"
  , NP.Seq 883 884
  , NP.Var 884 "too_deep"
  , NP.Seq 884 885
  , NP.Branch 885 (NP.Eq (NP.Id "too_deep") (NP.Num 1)) 887 887
  , NP.Seq 886 871
  , NP.Seq 886 888
  , NP.Var 887 "NOP_887"
  , NP.Seq 887 888
  , NP.Var 888 "IF_ELSE_FOOTER"
  , NP.Assign 889 "next_instr" (NP.Num 0)
  , NP.Seq 889 890
  , NP.Assign 890 "stack_pointer" (NP.Num 0)
  , NP.Seq 890 891
  , NP.Var 891 "word"
  , NP.Seq 891 892
  , NP.Assign 892 "opcode" (NP.Num 0)
  , NP.Seq 892 893
  , NP.Assign 893 "oparg" (NP.Num 0)
  , NP.Seq 893 894
  , NP.Branch 894 (NP.Eq (NP.Num 0) (NP.Num 1)) 895 898
  , NP.Var 895 "word"
  , NP.Seq 895 896
  , NP.Assign 896 "opcode" (NP.Num 0)
  , NP.Seq 896 897
  , NP.Assign 897 "oparg" (NP.Num 0)
  , NP.Seq 897 898
  , NP.Seq 897 894
  , NP.Var 898 "LOOP_FOOTER"
  , NP.Seq 898 899
  , NP.Seq 898 35
  , NP.Var 899 "__CLABEL_early_exit"
  , NP.Seq 899 900
  , NP.Var 900 "NOP_900"
  , NP.Var 901 "dying"
  , NP.Seq 901 902
  , NP.Assign 902 "frame" (NP.Num 0)
  , NP.Seq 902 903
  , NP.Assign 903 "undefed" (NP.Num 0)
  , NP.Seq 903 904
  , NP.Assign 904 "undefed" (NP.Num 0)
  , NP.Seq 904 905
  , NP.Assign 905 "return" (NP.Num 0)
  , NP.Seq 905 906
  , NP.Seq 905 906
  , NP.Var 906 "PROG_END"
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
  , WP.Seq 28 899
  , WP.Seq 28 30
  , WP.Var 29 "NOP_29"
  , WP.Seq 29 30
  , WP.Var 30 "IF_ELSE_FOOTER"
  , WP.Assign 31 "next_instr" (WP.Num 0)
  , WP.Seq 31 32
  , WP.Assign 32 "stack_pointer" (WP.Num 0)
  , WP.Seq 32 33
  , WP.Seq 32 814
  , WP.Seq 32 34
  , WP.Var 33 "NOP_33"
  , WP.Seq 33 34
  , WP.Var 34 "IF_ELSE_FOOTER"
  , WP.Seq 34 883
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
  , WP.Seq 67 814
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
  , WP.Seq 116 810
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
  , WP.Seq 200 810
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
  , WP.Seq 342 814
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
  , WP.Seq 383 810
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
  , WP.Seq 475 814
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
  , WP.Seq 643 814
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
  , WP.Seq 794 810
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
  , WP.Var 807 "NOP_807"
  , WP.Seq 807 808
  , WP.Var 808 "__CLABEL_CODEGEN_SWITCH_EXIT_0"
  , WP.Seq 808 809
  , WP.Var 809 "NOP_809"
  , WP.Var 810 "__CLABEL_pop_2_error"
  , WP.Seq 810 811
  , WP.Assign 811 "stack_pointer" (WP.Num 0)
  , WP.Seq 811 812
  , WP.Seq 811 814
  , WP.Var 812 "__CLABEL_pop_1_error"
  , WP.Seq 812 813
  , WP.Assign 813 "stack_pointer" (WP.Num 0)
  , WP.Seq 813 814
  , WP.Seq 813 814
  , WP.Var 814 "__CLABEL_error"
  , WP.Seq 814 815
  , WP.Branch 815 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 817 818
  , WP.Assign 817 "stack_pointer" (WP.Num 0)
  , WP.Seq 817 819
  , WP.Var 818 "NOP_818"
  , WP.Seq 818 819
  , WP.Var 819 "IF_ELSE_FOOTER"
  , WP.Branch 820 (WP.Eq (WP.Plus (WP.Num 0) (WP.Num 0)) (WP.Num 1)) 822 829
  , WP.Var 822 "f"
  , WP.Seq 822 823
  , WP.Assign 823 "stack_pointer" (WP.Num 0)
  , WP.Seq 823 824
  , WP.Branch 824 (WP.Eq (WP.Plus (WP.Id "f") (WP.Num 0)) (WP.Num 1)) 826 827
  , WP.Assign 826 "stack_pointer" (WP.Num 0)
  , WP.Seq 826 828
  , WP.Var 827 "NOP_827"
  , WP.Seq 827 828
  , WP.Var 828 "IF_ELSE_FOOTER"
  , WP.Seq 828 830
  , WP.Var 829 "NOP_829"
  , WP.Seq 829 830
  , WP.Var 830 "IF_ELSE_FOOTER"
  , WP.Seq 830 831
  , WP.Var 831 "__CLABEL_exception_unwind"
  , WP.Seq 831 832
  , WP.Var 832 "offset"
  , WP.Seq 832 833
  , WP.Var 833 "level"
  , WP.Seq 833 834
  , WP.Var 834 "handler"
  , WP.Seq 834 835
  , WP.Var 835 "lasti"
  , WP.Seq 835 836
  , WP.Var 836 "handled"
  , WP.Seq 836 837
  , WP.Branch 837 (WP.Eq (WP.Plus (WP.Id "handled") (WP.Num 0)) (WP.Num 1)) 839 843
  , WP.Var 839 "stackbase"
  , WP.Seq 839 840
  , WP.Branch 840 (WP.Eq (WP.Plus (WP.Num 0) (WP.Id "stackbase")) (WP.Num 1)) 841 842
  , WP.Var 841 "ref"
  , WP.Seq 841 842
  , WP.Seq 841 840
  , WP.Var 842 "LOOP_FOOTER"
  , WP.Seq 842 843
  , WP.Seq 842 871
  , WP.Seq 842 844
  , WP.Var 843 "NOP_843"
  , WP.Seq 843 844
  , WP.Var 844 "IF_ELSE_FOOTER"
  , WP.Var 845 "new_top"
  , WP.Seq 845 846
  , WP.Branch 846 (WP.Eq (WP.Plus (WP.Num 0) (WP.Id "new_top")) (WP.Num 1)) 847 848
  , WP.Var 847 "ref"
  , WP.Seq 847 848
  , WP.Seq 847 846
  , WP.Var 848 "LOOP_FOOTER"
  , WP.Seq 848 849
  , WP.Branch 849 (WP.Eq (WP.Id "lasti") (WP.Num 1)) 851 853
  , WP.Var 851 "frame_lasti"
  , WP.Seq 851 852
  , WP.Var 852 "lasti"
  , WP.Seq 852 853
  , WP.Seq 852 854
  , WP.Var 853 "NOP_853"
  , WP.Seq 853 854
  , WP.Var 854 "IF_ELSE_FOOTER"
  , WP.Var 855 "exc"
  , WP.Seq 855 856
  , WP.Assign 856 "next_instr" (WP.Num 0)
  , WP.Seq 856 857
  , WP.Var 857 "err"
  , WP.Seq 857 858
  , WP.Branch 858 (WP.Eq (WP.Plus (WP.Id "err") (WP.Num 0)) (WP.Num 1)) 860 860
  , WP.Seq 859 831
  , WP.Seq 859 861
  , WP.Var 860 "NOP_860"
  , WP.Seq 860 861
  , WP.Var 861 "IF_ELSE_FOOTER"
  , WP.Assign 862 "stack_pointer" (WP.Num 0)
  , WP.Seq 862 863
  , WP.Var 863 "word"
  , WP.Seq 863 864
  , WP.Assign 864 "opcode" (WP.Num 0)
  , WP.Seq 864 865
  , WP.Assign 865 "oparg" (WP.Num 0)
  , WP.Seq 865 866
  , WP.Branch 866 (WP.Eq (WP.Num 0) (WP.Num 1)) 867 870
  , WP.Var 867 "word"
  , WP.Seq 867 868
  , WP.Assign 868 "opcode" (WP.Num 0)
  , WP.Seq 868 869
  , WP.Assign 869 "oparg" (WP.Num 0)
  , WP.Seq 869 870
  , WP.Seq 869 866
  , WP.Var 870 "LOOP_FOOTER"
  , WP.Seq 870 871
  , WP.Seq 870 35
  , WP.Var 871 "__CLABEL_exit_unwind"
  , WP.Seq 871 872
  , WP.Var 872 "dying"
  , WP.Seq 872 873
  , WP.Assign 873 "frame" (WP.Num 0)
  , WP.Seq 873 874
  , WP.Assign 874 "undefed" (WP.Num 0)
  , WP.Seq 874 875
  , WP.Branch 875 (WP.Eq (WP.Plus (WP.Num 0) (WP.Id "FRAME_OWNED_BY_INTERPRETER")) (WP.Num 1)) 877 879
  , WP.Assign 877 "undefed" (WP.Num 0)
  , WP.Seq 877 878
  , WP.Assign 878 "return" (WP.Num 0)
  , WP.Seq 878 879
  , WP.Seq 878 880
  , WP.Var 879 "NOP_879"
  , WP.Seq 879 880
  , WP.Var 880 "IF_ELSE_FOOTER"
  , WP.Assign 881 "next_instr" (WP.Num 0)
  , WP.Seq 881 882
  , WP.Assign 882 "stack_pointer" (WP.Num 0)
  , WP.Seq 882 883
  , WP.Seq 882 814
  , WP.Var 883 "__CLABEL_start_frame"
  , WP.Seq 883 884
  , WP.Var 884 "too_deep"
  , WP.Seq 884 885
  , WP.Branch 885 (WP.Eq (WP.Id "too_deep") (WP.Num 1)) 887 887
  , WP.Seq 886 871
  , WP.Seq 886 888
  , WP.Var 887 "NOP_887"
  , WP.Seq 887 888
  , WP.Var 888 "IF_ELSE_FOOTER"
  , WP.Assign 889 "next_instr" (WP.Num 0)
  , WP.Seq 889 890
  , WP.Assign 890 "stack_pointer" (WP.Num 0)
  , WP.Seq 890 891
  , WP.Var 891 "word"
  , WP.Seq 891 892
  , WP.Assign 892 "opcode" (WP.Num 0)
  , WP.Seq 892 893
  , WP.Assign 893 "oparg" (WP.Num 0)
  , WP.Seq 893 894
  , WP.Branch 894 (WP.Eq (WP.Num 0) (WP.Num 1)) 895 898
  , WP.Var 895 "word"
  , WP.Seq 895 896
  , WP.Assign 896 "opcode" (WP.Num 0)
  , WP.Seq 896 897
  , WP.Assign 897 "oparg" (WP.Num 0)
  , WP.Seq 897 898
  , WP.Seq 897 894
  , WP.Var 898 "LOOP_FOOTER"
  , WP.Seq 898 899
  , WP.Seq 898 35
  , WP.Var 899 "__CLABEL_early_exit"
  , WP.Seq 899 900
  , WP.Var 900 "NOP_900"
  , WP.Var 901 "dying"
  , WP.Seq 901 902
  , WP.Assign 902 "frame" (WP.Num 0)
  , WP.Seq 902 903
  , WP.Assign 903 "undefed" (WP.Num 0)
  , WP.Seq 903 904
  , WP.Assign 904 "undefed" (WP.Num 0)
  , WP.Seq 904 905
  , WP.Assign 905 "return" (WP.Num 0)
  , WP.Seq 905 906
  , WP.Seq 905 906
  , WP.Var 906 "PROG_END"
  ]
