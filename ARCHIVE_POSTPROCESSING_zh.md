# 归档后的报告与后处理（0.0.28.9001）

## 1. 保存、搬迁、重新打开

```r
archive <- archive_fit(fit, "measurement-archive")
archive <- read_fit_archive("measurement-archive")
summary(archive)
fit_checks(archive)
parameter_summary(archive)
posterior_summary(archive)
```

搬迁整个归档目录后，用新路径重新打开。省略归档预算仍使用有限默认值；可用 postprocess_budget("read_fit_archive") 查看并显式调整。超限会拒绝，不会自动放宽。

## 2. 被动报告是已保留的快照

上述报告以及 print() 只读取句柄中的快照，不重新打开、扫描、哈希或刷新归档。即使归档后来不可访问，也仍可查看保留报告。状态 retained_snapshot_not_reverified 表示本次没有重新验证文件；缺失的缓存不会自动生成，历史不利证据也不会被清除。

## 3. 主动计算必须明确请求

refresh_diagnostics()、individual_parameter_summary()、ppc_plan() 和 assess_measurement_fit() 支持归档句柄。使用各接口已有的预算格式，明确提供有限的 streaming 后验读取预算；旧 reader 会拒绝，不会退回原来的路径。参数选择、预测层级和计算上限仍由调用者明确指定。完整预算字段和示例见 DIAGNOSTIC_REFRESH.md、INDIVIDUAL_PARAMETERS.md、BASIC_FAMILY_PPC.md。

每次主动调用（包括 PPC 规划）都会重新打开归档，核对句柄固定的 manifest，验证磁盘中的原始 portable 上下文，并对流式获取的最终后验快照核对归档时哈希。搬迁后的成员路径与历史拟合路径分开处理；归档时哈希不会变成拟合时来源证明。损坏、换写 manifest、超限或受保护的不利证据会导致明确拒绝。

## 4. 接住返回值，不会回写归档

诊断刷新和 PPC 返回带更新报告缓存的新归档句柄；输入句柄与归档目录里的文件不会改变。请把返回值赋给变量，必要时自行 saveRDS() 保存该句柄。个体摘要返回原有表格类型。重新 read_fit_archive() 会从磁盘原始 capsule 的缓存重新开始，不会找回只保存在先前句柄里的新缓存。PPC 获取失败沿用现有失败尝试缓存语义，不表示计算成功。

## 5. 科学边界

归档 PPC 支持 canonical Gaussian、binomial 和 lognormal 基本分布族，以及单一区块 pinned M3-CS；0.0.34.9000 候选新增恰好两个 canonical pinned M3-CS 的独立 joint profile（仅新导出对象，旧 capsule 不升级，R-only 证据）；都要求明确预测层级。M3-CS 必须显式提供一致的 rng_v2 simulator，主动检查会运行有预算限制的 fixed_param 响应复制，不会隐式编译或后验拟合。完整路由 M3、混合认知区块、三个及以上认知区块、SDM 和 EZ PPC 仍拒绝。双区块新范围见 SAVED_JOINT_M3CS_PPC.md。EZ 的严格 portable/归档导出已支持，但不赋予预测或 backend 能力；未实现 SAM 或 LOO。见 SAVED_COGNITIVE_WORKFLOW_zh.md。

归档只用于可信、本地产生的 RDS；哈希是意外损坏检查，不是签名或恶意输入安全保证。预算是有限逻辑工作量/存储上限，不是运行时峰值内存或耗时保证。
