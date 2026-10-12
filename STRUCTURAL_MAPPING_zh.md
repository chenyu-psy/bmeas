> 0.0.38.9000 更新：以下 .36 记录作为冻结增量历史保留。当前另有 component_prior_draws() 窄范围原生先验与可选限定 lavaan S3；详见 COMPONENT_PRIOR_DRAWS_zh.md、STRUCTURAL_LAVAAN_zh.md。S4 密度/BF 仍未实现。

# 可选结构后处理 S1/S2：联合总体协方差契约与解析回归

本增量为 0.0.36.9000。它不修改 measurement fitting，不运行 Stan/HMC，不自动启动 LOO，不实现正式 BF、KDE、lavaan 或原生联合先验 RNG。

## 实际审计结果

现有 `component_draws(fit)` 保存的是同一 retained state 的总体 `R_component` 与 `component_row_sd`，并据此形成协方差；不是个人 factor scores 的样本协方差。它有 `chain_id/saved_row`、稳定坐标 ID、marker/unit scale、orientation/GF2 sign-system、完整 prior specification、模型 fingerprint 及上游科学状态。原始 unavailable 标记、非精确对称和非 SPD 矩阵都可能保留。

现有 prior specification 不等于 prior draws。审计没有找到当前包的联合 population prior RNG。本增量不通过重命名 posterior 或拼接边缘抽样填补缺口。未来 RNG 必须按实际 prior chart、约束、量尺和完整联合参数化生成，另外验证；仅有总体矩阵后验不能计算 SDDR。

## 本次公开接口

- `population_draws(...)`：显式声明外部或合成的 joint population covariance array。需给出 distribution、稳定坐标/量尺/方向、model/prior ID、来源与 `joint_scope="joint_population_covariance_parameters"`。外部来源始终是 `user_declared_not_verified`，这些字符串不认证抽样的真实分布。
- `component_population_draws(fit, ...)`：通过现有受支持 reader 读取 posterior，保留整个 extraction；不执行 backend、不改变其科学状态。当前没有将任意 external prior 转成 native prior 的接口。
- `structural_draws(posterior_draws, predictors, outcomes, prior_draws=NULL, ...)`：独立可选后处理。沿现有 `component_draws` 命名风格，采用简短名称；原计划的 `bmeas_structural_draws` 是提案名称，未曾发布。
- `summary()`：在没有失败行时给描述性均值/SD/分位数；存在未映射行时不给筛选后摘要，仍保留每行状态。没有 ESS、MCSE、BF 或新的 identification/propriety 结论。

Covariance shape 为 D×K×K，两条变量轴必须与 `variables$id` 精确一致。`draw_keys` 必须是唯一的 `(chain_id,saved_row)`，但唯一 ID 不代表独立信息。相同矩阵值可以是真实的重复 MCMC state，保留并计数，不去重。相关矩阵缺少 SD/量尺不能自动当总体协方差；这个版本只接收 covariance 输入。

Prior/posterior 可有不同 D，不逐行配对。必须相同 model/prior identity、变量顺序、方向、量尺及完整 parameterization contract。独立的 external/synthetic 契约可以成对演示同一个映射；不能借此声称 native encompassing prior 已可用。用户可一致地重排两边变量；只换标签不换矩阵会被拒绝。

## 数值定义与失败处理

数值求解先归一到 correlation scale，再用 Cholesky triangular solve；非标准化输出采用检查 underflow/overflow 的 log-scale 单位转换。不会先让 unstandardized B 下溢为零后再计算 standardized B。代数上仍求 B=solve(Sigma_XX,Sigma_XY)，Psi=Sigma_YY-Sigma_YX B。默认原量尺；固定有名的 `units` 可改变单位或符号，且同样用于两边。Standardized B 使用各 draw 的 SD_X/SD_Y；standardized Psi 除以 SD_Y outer product。标准化与未标准化的假设不可无条件视作相同 BF。

必须整个声明域有限、可用、精确对称并正定。非正定、非对称、不可表示的单位变化、未解决方向和数值失败都保留行与 NA 槽位，给明确状态。无 jitter、nearest-PD、pseudoinverse、丢行或随机重试。总体矩阵或预测变量块的 rcond 小于固定阈值时为 `mapped_near_singular`；这不是自动拒绝或数值可靠性证明。

原生 orientation 只证明声明方向是否解决：普通 pinned/anchor 或剩余 GF2 basis 的零行；不将其升级为全局可识别。原生 adapter 保存整个 maps、prior、precision、original/derived availability 和上游证据。mapping 时还将派生对象与保留的 extraction 逐项核对，并重查 improper/non-normalizable 上游状态。这防止意外改写可用性、方向、量尺或矩阵，不是对同时一致伪造对象和 evidence 的密码学认证。传入 fit/archive 的现有验证仍由 component reader 负责。

默认每输入至多 10000 draws、128 variables；combined mapping 至多 10000000 数值槽与 100000000 声明矩阵工作单位，可显式调整。此契约不是 RSS 或墙钟保证；输入已经在内存中，reader acquisition 另有原预算。不会自动运行大型计算。

## 验证边界

2026-10-04 的 522 项历史 base-R 解析检查已复核脚本与 fixture，覆盖数学公式而非本 API。新增测试复用其 scalar grid 和 multivariate roundtrip 公式，另测当前契约、身份、量尺、方向、失败和 saved-reader 连接。本增量的精确执行证据由外层 validation 记录，不把历史 522 自动计入新接口通过数。

S3 lavaan 投影、S4 零点密度与正式 SDDR、S5 多维/乘积约束仍未实现。普通后验路径摘要可以独立使用，解释为 population regression functional，不是另一份 generative SEM likelihood。
