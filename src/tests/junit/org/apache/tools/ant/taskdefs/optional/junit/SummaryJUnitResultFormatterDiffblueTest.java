package org.apache.tools.ant.taskdefs.optional.junit;

import static org.junit.Assert.assertThrows;
import java.io.PipedOutputStream;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class SummaryJUnitResultFormatterDiffblueTest {
  /**
   * Test {@link SummaryJUnitResultFormatter#startTestSuite(JUnitTest)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SummaryJUnitResultFormatter#startTestSuite(JUnitTest)}
   */
  @Test
  public void testStartTestSuite_thenThrowBuildException() {
    // Arrange
    SummaryJUnitResultFormatter summaryJUnitResultFormatter = new SummaryJUnitResultFormatter();
    summaryJUnitResultFormatter.setOutput(new PipedOutputStream());

    // Act and Assert
    assertThrows(BuildException.class, () -> summaryJUnitResultFormatter.startTestSuite(new JUnitTest("Name")));
  }

  /**
   * Test {@link SummaryJUnitResultFormatter#endTestSuite(JUnitTest)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SummaryJUnitResultFormatter#endTestSuite(JUnitTest)}
   */
  @Test
  public void testEndTestSuite_thenThrowBuildException() throws BuildException {
    // Arrange
    SummaryJUnitResultFormatter summaryJUnitResultFormatter = new SummaryJUnitResultFormatter();
    summaryJUnitResultFormatter.setWithOutAndErr(false);
    summaryJUnitResultFormatter.setSystemOutput(null);
    summaryJUnitResultFormatter.setSystemError(null);
    summaryJUnitResultFormatter.setOutput(new PipedOutputStream());

    JUnitTest suite = new JUnitTest("Name");
    suite.setThread(0);

    // Act and Assert
    assertThrows(BuildException.class, () -> summaryJUnitResultFormatter.endTestSuite(suite));
  }
}
