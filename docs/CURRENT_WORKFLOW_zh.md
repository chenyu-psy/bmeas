# 当前工作流：0.0.47.9000 未发布候选

## 1. 声明、检查，再选择执行范围

用 bmf() 声明测量结构、bmeas(..., fit=FALSE) 构造规格，再查看 model_table()、get_prior()、check_model() 与 execution_plan()。按实际 backend support / qualification、科学错误及有限资源政策处理；experimental 不会取消证明的不识别、先验错误、完整性或预算限制。已有实际调用参数以各接口帮助页为准。

新增 common 和/或 parameter-residual product affine 仅开放 iid canonical Gaussian-only 的精确零分块 one-label 产品图。两层可同时为 product，各自保留标签与完整层 prior。需 execution="experimental"；qualified 明确拒绝。重复标签按完整区间交集映射，每个标签仅有一个 Jacobian；不能裁剪科学域。一般连通多自由标签与 centered/task/raw/custom/cognitive product 的执行不因本次开放。见 AFFINE_PRODUCT_CORRELATIONS.md。

## 2. 拟合后的报告与显式后处理

summary()、fit_checks() 与已缓存摘要是被动读取。refresh_diagnostics()、individual_parameter_summary()、component_draws() 等主动操作仍受各自已支持模型、完整源文件和有限预算约束。product 的导出、保留值后处理与当前 PPC reader 分别受各自 profile 和操作门槛约束，范围见第 3 节；被动缓存可读本身不授予主动执行能力。推断质量、先验/后验适当性和研究适用性必须分别判断。

## 3. M3-CS 与 Gaussian-product 的 PPC 路径

已有 live 单区块及 portable/搬迁归档单区块 reader 保持原契约。新 live 路径仅接受恰好两个 canonical pinned M3-CS 区块；不是任意混合认知模型。ppc_plan() 先检查完整设计预算，assess_measurement_fit() 显式传入 level、draws、seed、budget 与已有匹配 rng_v2 simulator。接口不会隐式编译或重拟合。

conditional 使用同一选中后验行的 joint eta；population 生成一次完整 iid 或 same-N centered 人员联合状态供两个区块共同使用。每次复制拼接为一次响应模拟，再依区块拆回；不按区块重抽潜变量或重设种子。概率接口返回区块命名矩阵列表，simulator data 附 row_map。预算包括全部十列响应和所有 block/task category covariance，不能缩小设计来绕过预算。见 JOINT_M3_CS_PPC.md。

0.0.34.9000 候选通过独立 joint-M3 producer/reader profile 开放新导出双区块 saved PPC，要求完整行/人员/操作数绑定；旧 profile 的双区块 capsule 仍拒绝。新增范围仅有 R-only 工程覆盖，见 SAVED_JOINT_M3CS_PPC.md。导出的被动证据或旧 schema 可读不等于取得执行能力。单区块保存工作流与其更窄的历史原生证据见 SAVED_COGNITIVE_WORKFLOW_zh.md。product affine fit 通过独立 saved-value profile 支持 portable/归档、被动报告及 component_draws/individual_parameter_summary。0.0.35.9001 候选的独立当前 reader 为完整既有 product capsule/搬迁归档开放 population / conditional PPC 及 ppc_plan()；diagnostic refresh 仍拒绝。原 capsule、producer capabilities（含 no-PPC 记录）、历史路径、源码与祖先 seal 均保留，不迁移、重分类或重新封印。范围见 PRODUCT_FIT_ARCHIVES.md 与 SAVED_PRODUCT_PPC.md。

product portable 的 ppc_plan() 仅做有界元数据/schema/绑定检查，完整性标记为 not_verified_by_this_metadata_call，不读取后验或验证 immutable digest。assess_measurement_fit() 才要求本次操作新完成的 context receipt、immutable digest 与受保护不利证据检查，并使用显式 level、draws、seed 和有限 streaming posterior budget；无需也不接受外部 Gaussian simulator。归档规划与评估每次均重新打开并验证归档权威成员，搬迁后的 CSV 按归档摘要核对；历史路径不重新绑定。新增 reader 仅有 R-only 工程覆盖，experimental / not_assessed 范围不变。

## 4. 结构后处理留在独立可选阶段

使用 component_population_draws(fit) 读取被支持的已保存联合成分后验。对满足窄门槛的当前 prepared spec，使用 component_prior_draws(spec, draws, seed) 生成同一模型的完整联合成分协方差先验边缘分布；然后将两者交给 structural_draws()，使用同一确定性解析回归映射。先验与后验的全部模型/先验/坐标/量尺/方向元数据必须完全一致。生成器仅支持普通完整 LKJ（1≤eta≤10000）、unit-SD 和已解析方向；其余范围见 COMPONENT_PRIOR_DRAWS_zh.md。现在可显式选择 backend="lavaan"，以固定的 lavaan 0.7.2 规则映射同一 prior/posterior；model=NULL 为饱和回归，限定别名语法可作有约束的观测协方差路径投影，详见 STRUCTURAL_LAVAAN_zh.md。不支持通用潜变量/递归/多组 SEM。已有 S4a 单系数 point-KDE/log-density-ratio 诊断接口 structural_sddr()，范围见 STRUCTURAL_SDDR_DIAGNOSTICS_zh.md；正式 BF 与 density MCSE 仍不可用。不得将协方差后验当成第二份数据 likelihood，也不默认运行 LOO、重拟合或 SEM。

## 5. 验证如何解释

此前 product saved-reader 的 R-only 回归按其冻结版本保留；本候选新增窄范围 native-R 先验、解析结构映射及 callback capture 回归。完整包检查尚须以单独候选验收记录为准，不继承前一冻结 release 的通过结论。四次 joint M3-CS cached fixed_param、common-product affine 门及 residual/dual cached-executable 原生门属于各自此前冻结分支，本次未重新执行。residual/dual 原比较器错误地期望 GQ 输出 transformed parameters，后经批准仅修正已保存输出比较，未改数据、容差或重跑原生执行。它们不证明后验来源、HMC、识别性、恢复、收敛、真实研究科学适用性或集群规模性能。原 M6 的集群参数恢复、覆盖率与研究验证仍待完成；本次工程验收不替代这些验证。分发内容不含原生二进制、序列化运行状态或真实数据。
