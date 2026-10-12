# 当前能力速查：0.0.47.9000 未发布候选

这里区分「可以声明/生成代码」「允许执行」「有何验证」「保存后能做什么」。支持接口不等于后验推断合格。具体组合应查看 check_model(spec)$backend 的 support / qualification 与明确错误；experimental 不能绕过硬性科学错误、来源检查或预算。

- canonical Gaussian、binomial-logit、lognormal：主 API 声明及支持的联合结构、准备/执行、诊断与个体摘要、严格 portable/归档、population / conditional PPC 已有相应工程覆盖。具体自由/固定/约束组合仍受各自 backend 门槛限制；不是任意层级的无条件保证。
- 单区块 pinned M3-CS：声明/准备、已有主 API experimental 执行路径、被允许的诊断/摘要与保存操作；live、portable、搬迁归档有显式 simulator 的 population / conditional PPC。不得省略完整人员名册、十列响应、来源或预算。当前 iid/centered/all-fixed 有 R-only 控制流程测试；此前 saved-reader 原生门只有 iid/centered，且早于最终导出修复。
- 恰好两个 canonical pinned M3-CS 区块：live population / conditional PPC 已支持；每次复制使用同一联合后验行、同一完整人员 eta 和一次拼接 rng_v2 调用。十列响应、全部 block/task 协方差和完整预算均保留。单区块输出不变；0.0.34.9000 候选新增独立 joint-M3 profile，支持新导出对象的 portable/搬迁归档 PPC；旧 profile 双区块 capsule 仍拒绝，不迁移或重新封印。新增证据仅为 R-only 控制流程，见 SAVED_JOINT_M3CS_PPC.md。此前冻结分支完成 4 次已有可执行文件的 fixed_param 工程调用，不是后验拟合或群体独立性检验。见 JOINT_M3_CS_PPC.md。
- 多标签 affine 产品图：仅 iid、canonical Gaussian-only、common-plus-residual 下，common 和/或 parameter-residual 层每个潜在非零连通块最多一个标签时允许明确 experimental 请求；其余层可用原有 MGS/fixed/one-label。两层各自保持独立标签与 prior。跨断开块重复标签共享区间交集与一个 Jacobian，完整层 determinant prior 不变。live 缓存报告可用；新增独立 product saved-value profile 支持 portable/归档、被动报告及 component_draws/individual_parameter_summary，保留原始证据与 seal；0.0.35.9001 候选通过独立当前 reader 为完整既有 product capsule/搬迁归档开放 population / conditional PPC 及 ppc_plan()，不改写原 producer 的 no-PPC 能力记录。diagnostic refresh 仍关闭；新增 reader 仅有 R-only 工程覆盖，inference 保持 not_assessed，见 SAVED_PRODUCT_PPC.md。qualified 与 centered/task/raw/custom/cognitive product 执行仍关闭，一般连通多自由标签图仍未实现。common-product 的 2440 项与 residual/dual 的 10225 项已保存输出比较是各自冻结状态的算术证据；后者未直接观察 GQ 中的 transformed R/L/D/u/eta/logJ，不证明科学适用性。见 AFFINE_PRODUCT_CORRELATIONS.md。
- 完整/路由 M3：保留单独的历史 measurement() 全路由及其原 G/P/C 范围，不应视为与当前 main-API pinned M3-CS reader 通用。历史 same-N centered-group simulator 的范围不自动扩大为新的 conditional 或 saved PPC。没有新的全路由 saved-reader 资格。
- SDM：有 hash-pinned 参考 likelihood 与明确范围的声明/编译/experimental 组合；保存、报告或诊断必须通过实际 schema 与已有消费者检查，不保证任意组合。SDM 响应模拟/PPC 未实现。
- revised EZ：ezdm_model() 是 pinned revised 三参数汇总似然，输入为 RT 均值/方差和正确计数/总数；不是通用 trial-level DDM。通用 experimental 执行门保持不变；已有固定状态及两人独立残差 alpha/SD/white 原生证据；这些历史 fixed-state 门未覆盖任意 factors/loadings/free correlations 或 HMC。后续工程证据见交付中的 2026-10-10 EZ 验证补充及 metric-window 更新，不代表 HMC、恢复或正式推断资格。严格 portable/归档导出与被允许的报告/诊断现已支持；EZ PPC、detached backend、source/standata 仍拒绝。
- typed custom row adapter：可声明并在已注册的编译器契约下请求 experimental 执行。数学正确性由注册者负责；没有通用 saved/PPC、原生或科学资格。模型名称或删除回调不会使它变成 canonical basic family。

## 保存、诊断与科学解释

报告接口是被动缓存；主动刷新/摘要/PPC 需要独立预算和源完整性检查。归档搬迁不会认证原始后验来源；更新结果只写返回副本。portable 不可代替 live backend，也不能重新生成数据/模型或重新封印旧 fit。见 SAVED_COGNITIVE_WORKFLOW_zh.md、PORTABLE_FITS.md 与 ARCHIVE_POSTPROCESSING_zh.md。

SAM、将成分协方差后验自动当成新结构似然、通用 SEM、LOO、通用模型恢复与真实研究科学验证都不是这次增量。早期场景评估、NEWS 和原生门是其冻结版本的历史证据；不要用旧「未实现」结论覆盖本页的新功能，也不要把新接口追溯成旧证据的新增验证。

## 下一阶段计划

S1/S2 的联合协方差契约与确定性解析回归映射已在限定范围实现。0.0.37.9000 新增完整普通 LKJ（1≤eta≤10000）、unit-SD、方向已解析的原生联合成分协方差先验生成器；不支持受限相关图、raw-B、marker/basis/share 或 parameter-residual 扩展。0.0.38.9000 新增可选 lavaan 0.7.2 的限定观测变量协方差回归/路径映射（S3）：与解析饱和回归区分、保留所有失败/边界行、同一固定 prior/posterior 映射；不是通用 SEM。0.0.39.9000 已实现 S4a 单系数 point-KDE/log-density-ratio 诊断；正式 SDDR BF 与 density MCSE 仍不可用；不将测量协方差后验当成新 likelihood。详见 COMPONENT_PRIOR_DRAWS_zh.md 与 STRUCTURAL_LAVAAN_zh.md。完整工作流见 CURRENT_WORKFLOW_zh.md。现有测试不代表真实研究、通用模型恢复、HMC 或集群规模性能已经验证。
