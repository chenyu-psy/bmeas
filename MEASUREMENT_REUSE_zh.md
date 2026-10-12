# 保存测量模型，供后续结构分析复用

本版新增两个层次：

- `component_draws()`：提取每个已保存联合状态中的成分相关矩阵和成分标准差，计算对应协方差，保留同一组链号与保存行号。
- `archive_fit()` / `read_fit_archive()`：保存并重新打开已有的完整后验 CSV、科学上下文和仍可取得的声明资源文件。整个目录可以搬迁，重新打开时校验文件。

以上操作都不会重新拟合测量模型。相关矩阵保留测量模型原来联合估计的结构，包括正交、选择性零相关、固定值和相等约束；不会把边缘均值拼成“联合抽样”。

## 1. 先明确保存对象

`fit` 可以是当前主接口拟合对象，或受支持的完整 `portable_fit()` 对象。原始 CSV 必须仍在。

旧版 live fit 若受包版本/序列化指纹限制，应先在原包环境导出 `portable_fit()`，再在新环境使用；不要自行修改指纹或源文件路径。`portable_fit()` 本身不会复制 CSV，完整搬迁请使用下面的归档接口。

## 2. 日常用法：保存、重新打开、提取

```r
archive <- archive_fit(fit, "measurement-archive")
archive <- read_fit_archive("measurement-archive")
joint <- component_draws(archive)
saveRDS(joint, "joint-components.rds")
```

省略预算时使用适合较小拟合的有限默认上限；不足会拒绝，不会自动加大上限、抽稀、重新拟合或更改源文件。这些上限不是内存峰值、RSS、耗时或推断可靠性的保证。

目标目录必须尚不存在。保存成功后，应保留整个目录；不要只复制 manifest 或其中一个 RDS。历史上没有保存的初始化 JSON、原始输入或缺失资源，不会被伪造补齐。

`joint$correlation`、`joint$sd`、`joint$covariance` 共用 `joint$draw_keys`。成分轴使用稳定 ID；`joint$coordinates` 和 `joint$maps` 保留显示名、尺度、方向和约束信息。只保存 `joint` 可以独立保留这些提取值，但不能替代完整测量归档。

协方差明确由已保存的 R 和 SD 派生。原始不可用标记不会因重新计算而消失；下溢、溢出和非正定状态有明确状态，不会被修补、删除或自动准许进入回归计算。

## 精度与推断边界

新主接口拟合默认 `control$csv_sig_figs = 17`，可显式设为 1–18。旧文件的 12 位输出仍是 12 位证据，转为 RDS 不会恢复丢失精度。17 位着眼于 double 往返保存，不等于矩阵条件良好或推断可靠。

归档保留已经保存的全部 CSV 列；成分相关/协方差不等于目标预测变量协方差，也不等于个体得分。逐抽样计算偏相关或标准化回归系数属于原测量后验的变换。另建有约束路径模型、换先验或定义一般 SAM 结构后验，还需要单独说明统计目标与推断方法。本版没有自动实现这一步。归档句柄支持明确请求的诊断刷新、个体参数摘要和基本分布族 PPC，流程和边界见 ARCHIVE_POSTPROCESSING_zh.md。

读取的是可信、本地产生的 RDS 归档。哈希用于发现意外损坏，不是签名；归档时哈希不能补充历史拟合时未记录的来源证明。更多限制和预算含义见 `MEASUREMENT_ARCHIVES.md` 与 `COMPONENT_DRAWS.md`。

## 进阶：查看与调整预算

```r
budget <- postprocess_budget("component_draws")
budget$max_components <- 6
joint <- component_draws(archive, budget = budget)
```

`postprocess_budget("archive_fit")` 和 `postprocess_budget("read_fit_archive")` 返回相同归档上限；每次返回独立普通列表，修改不会影响未来调用。操作名必须完整。显式预算仍按原规则验证，不合并缺失字段，`NULL` 也不代表默认值。

完整默认数值如下，便于审阅和精细调整：

```r
posterior_budget <- list(
  reader = "streaming",
  max_source_bytes = 256 * 1024^2,
  max_read_bytes = 7 * 256 * 1024^2,
  max_materialized_values = 10000000,
  max_record_bytes = 1048576,
  max_columns = 100000,
  max_source_rows = 100000,
  max_chains = 16,
  max_temp_bytes = 2 * 256 * 1024^2,
  max_scan_values = 100000000
)
context_budget <- list(
  max_context_bytes = 16 * 1024^2,
  max_context_nodes = 2000000,
  max_depth = 64
)
archive_budget <- list(
  max_members = 64,
  max_member_bytes = 256 * 1024^2,
  max_total_bytes = 512 * 1024^2,
  max_context_bytes = 16 * 1024^2,
  max_read_bytes = 8 * 1024^3,
  max_temp_bytes = 2 * 1024^3,
  posterior = posterior_budget,
  context = context_budget
)
component_budget <- list(
  posterior = posterior_budget,
  max_components = 12,
  max_output_values = 10000000,
  max_matrix_work = 100000000,
  context = context_budget
)
```
