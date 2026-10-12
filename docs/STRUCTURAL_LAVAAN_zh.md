# 可选结构后处理 S3：限定的 lavaan 协方差路径映射

0.0.38.9000 增量；不修改测量拟合，不运行 HMC/Stan，不计算 S4 密度或 BF。lavaan 是可选 Suggests 依赖，仅在显式选择 `backend="lavaan"` 时要求安装已验证的 **0.7.2**。其他版本明确拒绝；未安装不影响默认解析映射。

## API 与限定语法

`structural_draws(posterior_draws, predictors, outcomes, prior_draws=NULL, coefficient_scale="unstandardized", units=NULL, control=list(), backend="lavaan", model=NULL, backend_control=list())`。

默认 backend 仍为 `analytic`，既有参数位置及解析结果保持不变。模型为 NULL 时自动建立所有 Y~X 路径、完整自由 X 协方差和完整自由 Y 残差协方差，输出 `regression_functional`，与 S2 解析公式对应。

显式 model 使用确定性的 `x1...xP`、`y1...yQ` 别名，分别对应传入 predictors/outcomes 的顺序，不使用可能含冒号或其他特殊符号的原始坐标 ID。返回 `mapping_spec$aliases` 保存完整对应。允许如：

```
model = "y1 ~ x1 + x2; y2 ~ x2; y1 ~~ 0*y2"
```

只支持 `Y ~ X`、`Y ~~ Y`，每项可自由或以有限数字固定，如 `0*x1`、`1e-3*y1`。显式模型中遗漏的路径和非对角残差协方差固定为零；遗漏的残差方差保持自由；X 协方差始终完全自由。重复项（包括反向重复协方差）拒绝。即使显式模型恰好饱和，仍保守标记 `projected_path`。

不支持潜变量、递归/结果预测结果、均值/截距、阈值/类别数据、多组、多层、标签或 equality/inequality 约束、定义参数、随机起点、外部优化器参数及任意完整 lavaan 语法。此接口是限定协方差回归/路径后处理，**不是通用 SEM 后端**。

## 同一固定映射

在 prior/posterior 前共同构造一份模型、变量/别名/量尺、固定单位、参数表/抽取键、目标、明确选项和版本默认快照。每次拟合核对解析后的自由/固定参数契约和选项；记录实际 resolved options 并检查所有成功解析的 draws 及 prior/posterior 一致。记录 R/platform/BLAS。未拟合任何行时 resolved options 可为空，不能声称已验证运行选项。

固定为单组 normal-ML covariance discrepancy：`sample_cov_rescale=FALSE`、无 meanstructure、`fixed_x=FALSE`、`conditional_x=FALSE`、`se="none"`、`test="none"`、不计算 baseline/H1；优化器 nlminb，`optim_attempts=1`、`rstarts=0`、`start="simple"`、不强制收敛、无 bounds、无逐行 warm starts 或随机重试。当前 retry policy 为不重试。模型中所有允许协方差项显式列出，不依赖自动自由项。

backend_control 只允许 `max_fits=100`、`reference_n=1000`、`iter_max=1000`、`eval_max=2000`、`rel_tol=1e-10`、`boundary_tol=1e-8`。这些选项一次固定后同时应用于 prior/posterior，不允许依据某个 draw 的结果自适应改变。max_fits 在拟合前按两侧总行数保守预算；迭代/评估次数不等于墙钟保证。原有 control 仍控制输入维数和数值工作量。

reference_n 只是 lavaan API 所需的共同常数，不是新样本量、不产生新精度。目标函数仅作为确定性协方差差异最小化，不作为第二份数据 likelihood。固定小矩阵已验证 N=50/5000 结果相同；这不是所有模型/数值条件下的普遍证明。返回目标值和协方差残差仅供投影/数值诊断，不能当作模型检验、SE、p 值或生成式 SEM BF。

标准化用输入总体协方差在固定单位变换后的 SD_X/SD_Y；残差协方差除以输入 SD_Y 的外积。尤其在投影模型中，不用 lavaan `std.all` 代替这个定义。原量尺直接优化，不对每个 draw 偷换成另一个标准化约束模型。

## 失败、边界和解释

先运行与解析映射相同的全声明域/方向/有限性/精确对称/SPD/单位可表示性和解析可表示性检查，作为保守前提；因此某些极端量尺下本可用另一种方法求解的投影也会明确失败。无 jitter、nearest-PD、伪逆或删行。

每行保留原 draw ID 和 NA 槽位；失败/不收敛/非有限解/非正定残差或模型协方差不能成为 mapped。残差协方差以输入 Y SD 做有防护的标准化；其最小特征值不大于 boundary_tol 时标记 `lavaan_boundary_solution`，保留候选值但阻止普通摘要。该阈值是数值警戒线，不是数学上必然处于精确边界的证明。无法表示或无法完成边界检查同样保留失败行。

保存警告、错误、候选未标准化解、收敛、迭代数、目标、梯度、协方差残差和边界/可接受性字段。候选值不是获准映射值，特别是非收敛返回值可能只是起点；不得拿它们绕过 NA/summary 状态。

单次局部优化不能证明不存在其他极值。逐行标记 `global_optimum="not_verified"` 和 `local_optima_check="not_performed"`；不由收敛升级可识别性或上游科学状态。有任何失败/边界行即不给过滤后的描述性摘要。S4 仍未实现；以后正式 BF 还需规则性、条件先验及缺失映射等单独检查。投影路径 functional 为零的比较不能自动解释为独立生成式 SEM 比较。

## 依据与验证边界

- https://lavaan.ugent.be/tutorial/cov.html：协方差输入及默认 (N−1)/N 缩放；当前网页可能更新到较新版本。
- https://www.lavaan.ugent.be/history/dot7.html：0.7-2 参数迁移至 snake_case；旧 dotted aliases 已弃用。
- https://www.lavaan.ugent.be/tutorial/est.html：normal/Wishart 目标/缩放说明。
- 实际安装 0.7.2 的 help、选项、源码及外层 validation receipts 决定本增量的精确软件证据。

小型确定性 fixture 覆盖解析单/多元、输入 SD 标准化、别名/重排/单位、prior/posterior 同选项、参考 N 与 rescale 反例、限制投影、失败/边界保留及语法拒绝；不以这些测试声称泛化 SEM、真实数据测量估计或 SDDR 已验证。
