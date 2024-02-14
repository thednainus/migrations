<beast version='2.7' namespace='phydyn.model:beast.base.util:beast.base.math:beast.base.evolution.alignment:beast.base.evolution.tree'>

  <model spec='PopModelODE' id='simpleHIVmodel' evaluator="compiled" popParams='@initValues' modelParams='@rates'>
    <definition spec='Definition'>beta = if (t&gt;=1980) then max(0.0, a1980*t + b1980) else if ((t&gt;=1995) and (t!&gt;2005)) then max(0.0, b1980 + (a1980 - a1995) * 1995.0 + a1995 * t) else max(0.0, (2005.0 * (a1995 - a2005)) + b1980 + (1995.0 * (a1980 - a1995)) + a2005 * t)</definition>
    <definition spec='Definition'>alpha = if ((t!&gt;1995)) then 0.0 else max(0.0, a*t + b)</definition>

    <matrixeq spec='MatrixEquation' type="birth" origin="src" destination="src"> srcGrowthRate*src  </matrixeq>
    <matrixeq spec='MatrixEquation' type="birth" origin="I" destination="I"> beta*I</matrixeq>
    <matrixeq spec='MatrixEquation' type="migration" origin="src" destination="I"> srcMigrationRate*I </matrixeq>
    <matrixeq spec='MatrixEquation' type="death" origin="I"> gamma*I + alpha*I </matrixeq>
    <matrixeq spec='MatrixEquation' type="nondeme" origin="Tr">alpha*I</matrixeq>
  </model>


    <rates spec="ModelParameters" id='rates'>
      <param spec='ParamValue' pname='a' pvalue='-0.000014'></param>
      <param spec='ParamValue' pname='b' pvalue='0.8'></param>
      <param spec='ParamValue' pname='a1980' pvalue='0.0005'></param>
      <param spec='ParamValue' pname='b1980' pvalue='1'></param>
      <param spec='ParamValue' pname='a1995' pvalue='-1.98'></param>
      <param spec='ParamValue' pname='b1995' pvalue='5.47'></param>
      <param spec='ParamValue' pname='a2005' pvalue='0.36'></param>
      <param spec='ParamValue' pname='gamma' pvalue='0.1976285'></param>
      <param spec='ParamValue' pname='srcMigrationRate' pvalue='0'></param>
      <param spec='ParamValue' pname='srcGrowthRate' pvalue='0.3333333'></param>

    </rates>


    <trajparams id='initValues' spec='TrajectoryParameters' method="classicrk" integrationSteps="1001"  t0="1980" t1="2020">
      <initialValue spec="ParamValue" pname='I' pvalue='1'  />
      <initialValue spec="ParamValue" pname='src' pvalue='10000'  />
      <initialValue spec="ParamValue" pname='Tr' pvalue='0.0' />
    </trajparams>

    <run spec='TrajectoryOut' model='@simpleHIVmodel' file="trajectory.csv">
    </run>

</beast>
