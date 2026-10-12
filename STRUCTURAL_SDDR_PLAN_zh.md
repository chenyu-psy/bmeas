> .39 更新：新增 S4a 单系数 point-KDE/log-density-ratio 诊断；正式 BF 与 MCSE 仍不可用。最新范围见 STRUCTURAL_SDDR_DIAGNOSTICS_zh.md。下文保留旧版本路线和范围记录。

> 历史状态（2026-10-10，0.0.38.9000）：以下为原始路线图记录。S1/S2、窄范围 native prior、限定的可选 lavaan S3 已有实现，分别见 STRUCTURAL_MAPPING_zh.md、COMPONENT_PRIOR_DRAWS_zh.md、STRUCTURAL_LAVAAN_zh.md。S4/S5 与通用 SEM 仍不在本增量范围；旧提案不能当作实际 API。

# bmeas 可选结构后处理与 SDDR 计划

日期：2026-10-10。历史状态（0.0.37.9000）：S1/S2 已在限定范围实现，含外部声明契约、已有 component posterior adapter、确定性解析回归及窄范围完整 LKJ/unit-SD 原生先验生成。S3 lavaan、S4 密度/SDDR 与 S5 仍待实施。实际接口及限制见 STRUCTURAL_MAPPING_zh.md、COMPONENT_PRIOR_DRAWS_zh.md。以下保留早期提案的数学约定、后续里程碑和历史验收记录；提案 API 名称不等于当前导出名称。本次未运行测量拟合、Stan/HMC、LOO 或正式 BF。

## 目标与定位

在 measurement-only 拟合完成后，增加独立、可选的 structural postprocessing 阶段：将同一测量模型的联合先验与联合后验总体协方差抽样，逐份通过同一个确定性结构映射，得到路径系数的诱导先验与后验，再计算适用的 Savage–Dickey density ratio。lavaan 是可选映射后端，不承担先验或后验采样。

首次交付聚焦一个普通回归系数的零假设；保留原测量拟合对象、联合抽样及测量诊断。结构分析不反向改变测量估计，不自动重拟合数据，也不引入第二份数据 likelihood。

## 方法约定

- 定义 β = g(Σ)。prior/posterior 使用完全相同、预先固定的变量顺序、方向、量尺、标准化和拟合规则。各矩阵内部保留同一 draw 的全部联合依赖；先验与后验的 draw 数不必相同，也不需要逐行配对。
- H0 是原 encompassing measurement model 中 β=0 的限制；其余所有参数采用该模型在 β=0 下诱导的条件先验。由此定义兼容的零模型，无需用户另外重建一套 nuisance prior。若用户另行指定零模型先验，普通 SDDR 不自动适用。
- BF01 = posterior density at 0 / prior density at 0；BF01 > 1 支持 H0，BF10 为倒数。这里计算密度，不是抽样恰好等于零的比例。
- 不要求 Σ→β 全局可逆。要求映射明确定义、目标可识别，零点附近具有适当连续密度和条件先验；首版限 proper priors、有限正的先验零点密度及有限边际 likelihood。
- 若 lavaan 拟合过度识别的结构，结果可解释为固定拟合准则下的投影路径 functional。其 SDDR 可检验该 functional 为零；它不自动成为两个独立生成式 SEM 的模型比较。输出明确标注 `regression_functional` 或 `projected_path`，不把后一类一概拒绝。
- 非标准化和依赖未知 SD 的标准化系数分开定义假设。即使零集合相同，条件先验也可能不同；不能无条件声称两者 BF 相同。

## 里程碑与验收

### S1：联合抽样契约与先验可用性审计

先检查现有对象是否保存总体 Σ、相关矩阵及 SD、链号/draw ID、变量名、因子方向、量尺、先验规格及模型来源。审计是否已经能生成对应的完整联合参数 prior draws；若没有，记录最小新增导出/RNG 需求及其授权范围，不在本计划内执行。

验收：以固定构造对象覆盖变量错序、缺失尺度、非对称/非正定矩阵、混合模型来源、缺失元数据；应明确拒绝或返回诊断。相关矩阵仅在目标映射信息充分时接受。不能拼接独立边缘抽样，也不能将个人因子分数的样本协方差或 prior-predictive 数据拟合分布冒充总体参数抽样。

### S2：解析回归映射基线

先定义 β = solve(ΣXX, ΣXY)，残差协方差为 ΣYY − ΣYX β；标准化时使用每份矩阵对应的 SD。输出保持所有 draw ID，附输入/输出映射及失败状态。

验收：固定正定矩阵的解析往返、单/多预测变量、变量重排与固定单位变换、近奇异矩阵告警；prior 与 posterior 调用相同映射。输入完全相同时输出应相同。重复复制 draws 不得被当成新增独立信息。

已有方法文档记载 2026-10-04 的 522 项确定性解析检查；这是历史记录，本次未重跑，也不作为新接口或实际测量模型 BF 已验证的证据。未来实施应复核原脚本与 fixture 后复用，避免重复造轮子。

### S3：lavaan 等价映射与投影模式

用可选 lavaan 后端接入 covariance→coefficient。固定模型语法、优化目标、识别方式、变量尺度、边界规则、初始化/重启策略及版本。关闭 covariance 默认 (N−1)/N 重缩放；关闭或忽略普通 SE、检验和 p 值。

验收：在可识别饱和回归 fixture 中，与 S2 系数及残差结果逐 draw 比较；预先记录绝对/相对容差（建议 1e−6 起，结合求解精度解释）。仅为 API 所需的 N 不得充当新样本量，检验改变共同目标缩放不会实质改变映射。多组中影响权重的 N/权重必须显式固定。过度识别 fixture 标注为投影，并记录固定损失下的残差/收敛情况。

优化失败、Heywood/边界解、多解和局部最优都保留记录，不能静默删除后计算 BF。先尝试预定的确定性修复规则；仍缺少映射的 draws 时默认不给出正式 BF，并说明缺失可能改变分布。不得把失败后筛选出的 prior/posterior 冒充原分布。

### S4：单系数零点密度与 BF 诊断

对诱导 prior/posterior 的同一系数估计零点密度。首版提供一种明确命名、可复现的 KDE 估计器及带宽敏感性检查；不自动假设系数服从正态。使用 log BF 避免上下溢，附零点密度、分批/分链波动、相关性/ESS 诊断、带宽范围和零点邻域支持情况。

验收：先在已知方差、正态先验的简单回归 fixture 中，将解析 SDDR 与独立边际正态 likelihood 比核对；明确这是计算验证，不是重跑测量模型。未来获准执行密度估计测试时，使用可控合成 draws 核对解析密度及 Monte Carlo 稳定性。覆盖 prior=posterior 时 BF=1、BF01/BF10 方向、零点远尾部、边界/奇异密度，以及相关 nuisance prior 的兼容/不兼容正负对照。不稳定时输出诊断与“不足以可靠估计”，不能输出未经支持的极端 BF。

### S5：后续扩展边界

多条路径同时为零需要联合密度，不能相乘 marginal BF；首版明确返回未支持。间接效应乘积、奇异零点、多维 equality/order 约束、独立指定的零模型先验及整个生成式 SEM 的模型比较分别列入后续研究，不由单路径验证背书。常规路径后验摘要可独立使用，无需先完成 BF 阶段。

## 暂定 API 提案（尚不存在；实施前核对命名与对象规范）

1. `bmeas_structural_draws(prior_draws = NULL, posterior_draws, model, backend = c("regression", "lavaan"), coefficient_scale = c("unstandardized", "standardized"), control = list())`
   - 输入为经 S1 验证的总体联合协方差 draw 对象；`model` 为明确预测变量/结果变量的回归规格或 lavaan 语法。
   - posterior-only 可得到描述性路径摘要；缺少 prior 时不提供 BF。
   - 返回 `prior`、`posterior`、`draw_status`、`mapping_spec`、`estimand_type`、`provenance`、`diagnostics`；保存 draw ID、系数名、尺度及映射选项。
2. `bmeas_structural_sddr(x, parameter, null = 0, null_prior = "encompassing_conditional", density = "kde", control = list())`
   - 首版 `parameter` 必须是单个普通系数；`null_prior` 声明此接口实际比较的零模型，不是一个可绕过检查的“认证”开关。
   - 返回 `log_bf01`、`bf01`、`bf10`、`prior_density_at_null`、`posterior_density_at_null`、`hypothesis_definition`、`density_diagnostics`、`status`。
   - 记录兼容条件先验的定义、映射来源与已验证范围；对缺失/不匹配 draws、未解决失败或不稳定密度，不输出正式数值 BF。

## 集成顺序与完成定义

建议作为现有 measurement-only 与模型支持路线后的独立可选里程碑：S1/S2 → S3 → S4；S5 留在后续研究。lavaan 保持可选依赖，不影响未安装用户的核心拟合。本文件作为 0.0.32.9002 的独立规划文档，由 CURRENT_CAPABILITIES_zh.md 与 CURRENT_WORKFLOW_zh.md 引用；没有新增结构函数。S14/S15 的精确分块 affine 增量及其受限 experimental 执行不等于一般结构图或推断验证，本计划不将其视为 SDDR 已具备的基础或验证结果。

计划交付完成：保存本文件并供下一 release 的文档整合。实现完成另行定义为上述契约/数值/诊断测试通过、示例清楚区分 functional 与生成式 SEM、旧测量核心行为不变。当前不声称已有 API、测试通过或模型运行结果。

## 方法依据

- [Mulder, Wagenmakers & Marsman：SDDR 条件先验要求与修正项](https://arxiv.org/html/2004.09899)：支持 BF 方向及零模型 nuisance prior 兼容条件。
- [Wetzels, Grasman & Wagenmakers 2010：encompassing prior 推广](https://www.sciencedirect.com/science/article/pii/S0167947310001180)：支持 equality restriction 的先验/后验密度思路；精确约束需明确条件化方式。
- [Verdinelli & Wasserman 1995](https://doi.org/10.1080/01621459.1995.10476554)：不同零模型先验需要普通密度比以外的修正。
- [lavaan 官方 covariance input](https://lavaan.ugent.be/tutorial/cov.html)：支持 covariance 输入，说明 N 与默认 covariance rescaling。实施时按实际安装版本确认参数拼写。

本文对 bmeas 推前/投影 functional 的结论是将上述原理应用于拟议流程的数学推论，不是声称文献已验证此软件实现。历史内部方法说明仅作已有工作索引：`CSMU_downstream_structural_posterior_SDDR_zh.md`；本次仅阅读，未改写或重跑其中检查。
