# 保存的认知测量模型：0.0.31.9001

## 支持范围

- portable fit 与重新打开的归档支持一个 canonical pinned M3-CS 区块的 population / conditional PPC。保存的生产者能力、原 capsule、身份、历史路径、封印和不利证据不改写；这是独立 reader 能力，不是迁移或重新封印。
- iid、完整人员名册 sample-centered 与 all-fixed 的声明、规划和控制流程有 R-only 对照。all-fixed 使用零 latent work，不能据此宣称已有原生执行验证。
- 严格 pinned revised EZ 科学来源记录现在可以完整导出到 portable/归档。未知字段或不合契约的值仍拒绝。EZ PPC、portable backend、源码生成和 standata 仍拒绝。
- 完整/路由 M3、三个及以上认知区块、认知与基本族混合、SDM、任意适配器均不因此获得 saved PPC。0.0.34.9000 候选为恰好两个 canonical pinned M3-CS 的新导出对象增加独立 profile；旧 profile 不升级，新增范围仅有 R-only 证据。见 SAVED_JOINT_M3CS_PPC.md。

## 工作流

先在原环境、原 seal 与 fit 关联仍有效时导出；普通旧版 fit RDS 不是自动跨版本迁移。

```r
library(bmeas)
portable <- portable_fit(fit, budget = list(
  max_context_bytes = 8 * 1024^2,
  max_context_nodes = 1000000, max_depth = 64))
archive <- archive_fit(fit, "measurement-archive")
# 搬迁完整目录后，用新路径明确重新打开。
archive <- read_fit_archive("new-location/measurement-archive")
summary(archive)
fit_checks(archive)
```

上面 fit 是已有拟合。导出/归档不会重新拟合。完整 CSV 和必要资源须仍可读取；portable_fit 必须提供有限 context 预算；上述值仅作示例。archive_fit / read_fit_archive 省略预算时采用有限默认值。超限应明确调整而非绕过。

主动 M3-CS PPC 还需要调用者已有、受信任且匹配的 bmeas_m3cs_rng_v2 simulator，以及适合完整设计的显式诊断/streaming 预算：

```r
# budget 使用 diagnostic_budget(...) 与 reader="streaming" 的有限后验预算。
# simulator 是调用者明确提供的匹配 rng_v2 能力；本包不分发原生二进制或运行状态。
plan <- ppc_plan(archive, draws = 2, level = "population", budget = budget)
checked <- assess_measurement_fit(archive, draws = 2, level = "population",
  seed = 20261008, budget = budget, simulator = simulator)
fit_checks(checked)
```

请根据 M3_CS_PPC_zh.md 与 SCALABLE_PPC.md 设置完整预算，包含十列响应、完整人员名册、源读取和统计工作量。portable 规划仅检查保存的声明和有界 schema，不认证摘要，也不访问资源/backend；归档规划仍会重新打开并验证归档。执行前检查 canonical 源码、资源和 executable 摘要；这些是提供能力/当前文件的一致性检查，不是独立源码到二进制证明。资源搬迁可在显式 simulator 的 resource_file 字段提供当前匹配路径；归档使用验证过的 manifest 资源映射。不会自动编译、找回后端或重新拟合。

返回 checked 是更新报告缓存的新副本。原句柄、capsule 和归档文件不改变。重新 read_fit_archive() 从磁盘原 capsule 的缓存重新开始；如需保留新缓存，调用者应保存返回句柄。conditional 只检查响应层，不能解释为总体结构拟合。失败尝试的 receipt 不表示成功。

## 原生与工程证据的边界

此前单独批准的 saved-reader 原生 seam 门，在导出修复之前的冻结源码上完成：iid / centered × population / conditional × live / portable / relocated archive 共 12 个 assessment cells；每格 2 次，合计 24 次已记录 fixed_param 调用，64.57 秒。它没有测试 all-fixed，也没有对本次最终导出修复源码重新执行原生模型。

原 strace/ptrace 尝试在 R 启动前被拒绝；经批准改用普通自身进程日志及 CmdStan 已存命令/参数、JSON、CSV 与退出码。因此这些记录不是 OS 级进程总数证明，也不是源码到二进制的独立认证。原预算 250000 slots 不足，经批准只将该项调整到 276303；保存输出审计原先按原始行位置对比，经 ID 对齐后通过，未改模型或输出。

本次 release 只做 R-only 安装、测试和标准源包检查；准确终止状态见交付顶层 VALIDATION.md。没有新增原生编译、模型执行、HMC、真实数据拟合。工程对照不证明后验来源、推断质量、恢复、识别性、科学适用性或大型研究性能。
