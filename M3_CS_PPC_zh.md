# M3-CS 预测检查：0.0.32.9002

0.0.32.9002 补充：live 恰好双区块已支持，完整联合状态、拼接响应、区块统计及证据边界见 JOINT_M3_CS_PPC.md。portable/归档仍限单区块。

以下示例为主 API 的单个内置、固定资源哈希 M3-CS 区块；live 恰好双区块另见上面的新增范围。保留原始 cs_logits 数值稳定化、十列响应、选项和行总数。

已有主 API 拟合 fit 与原始资源可用时：

```r
budget <- diagnostic_budget(
  max_draws = 21, max_observations = 1001,
  max_simulated_values = 210210,
  max_latent_values = 30000, max_statistics = 300,
  max_pair_terms = 1000000, max_work_values = 2000000,
  posterior = list(reader = "streaming",
    max_source_bytes = 128 * 1024^2,
    max_read_bytes = 7 * 128 * 1024^2,
    max_materialized_values = 2000000))
plan <- ppc_plan(fit, draws = 21, level = "conditional", budget = budget)
print(plan)
# 以下显式编译步骤需要可用的 CmdStan 工具链。
simulator <- compile_m3cs_simulator(fit$spec,
  cache_dir = "m3cs-simulator-cache", budget = budget)
checked <- assess_measurement_fit(fit, level = "conditional", draws = 21,
  seed = 20261008, budget = budget, simulator = simulator)
fit_checks(checked)
```

预算须按完整设计调整；十列响应全部计入工作量。上述数值仅为用法示例，不保证足够。ppc_plan 不读资源或后验文件、不编译、不采样；实际检查需要显式 simulator，绝不隐式编译或拟合。每次复制运行一个单线程 fixed_param 抽样，零预热、零模型参数。

- conditional 使用逐行对齐的联合 eta；其关联检查不能解释为总体测量结构拟合。
- population 使用实际 iid 或同 N 样本中心化的完整人员名册生成机制，包括无观测人员。
- 同人同任务的重复行先合并；正总数人员等权计算类别比例均值和样本标准差。跨任务全部类别对使用实际重叠人员、分母 overlap−1。三个任务产生 105 项统计量。
- 区间是描述性筛查，不是校准检验或合格判决；工程对照不证明模型恢复、统计功效或推断质量。
- 不支持完整路由 M3、SDM、ezDM、任意适配器或旧 G/P/C 映射。portable fit 与搬迁后的归档支持单一区块 pinned M3-CS 的规划与显式重新检查；必须另行提供一致的 rng_v2 simulator。详见 SAVED_COGNITIVE_WORKFLOW_zh.md。
- 源码、原始资源和可执行文件必须匹配。原始外部资源、可执行文件和模拟状态均不随本包分发。

保留已有完整 CSV 归档与联合成分提取；archive_fit、read_fit_archive、component_draws 可省略 budget，使用 postprocess_budget(operation) 的有限默认值。超限仍拒绝执行，不会静默截断；预测检查预算契约保持显式。

当前模拟器标识为 bmeas_m3cs_rng_v2；旧 v1 可执行文件不会被静默复用。有限默认值仍保留；主 API 的显式 streaming 预算可允许超过 20 次复制，更大完整设计仍须通过所有工作量检查。源码仅移除了开发期 O<=1000 的固定上限，未改变科学约束。

独立微型原生工程门已通过：一次编译、O=1 和 O=1001 各一次 fixed_param 调用；均为单链、单线程、一次抽样、零预热、零参数。检查涵盖概率、计数、支持范围、行总数与身份凭据，不是后验拟合、大规模性能或推断质量验证。详见 SCALABLE_PPC.md。
