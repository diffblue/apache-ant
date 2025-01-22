package org.apache.tools.ant.taskdefs.optional.junit;

import static org.junit.Assert.assertThrows;
import java.io.PipedOutputStream;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class PlainJUnitResultFormatterDiffblueTest {
  /**
   * Test {@link PlainJUnitResultFormatter#startTestSuite(JUnitTest)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PlainJUnitResultFormatter#startTestSuite(JUnitTest)}
   */
  @Test
  public void testStartTestSuite_thenThrowBuildException() throws BuildException {
    // Arrange
    PlainJUnitResultFormatter plainJUnitResultFormatter = new PlainJUnitResultFormatter();
    plainJUnitResultFormatter.setOutput(new PipedOutputStream());

    // Act and Assert
    assertThrows(BuildException.class, () -> plainJUnitResultFormatter.startTestSuite(new JUnitTest("Name")));
  }

  /**
   * Test {@link PlainJUnitResultFormatter#endTestSuite(JUnitTest)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PlainJUnitResultFormatter#endTestSuite(JUnitTest)}
   */
  @Test
  public void testEndTestSuite_thenThrowBuildException() throws BuildException {
    // Arrange
    PlainJUnitResultFormatter plainJUnitResultFormatter = new PlainJUnitResultFormatter();
    plainJUnitResultFormatter.setSystemOutput(null);
    plainJUnitResultFormatter.setSystemError(null);
    plainJUnitResultFormatter.setOutput(new PipedOutputStream());

    // Act and Assert
    assertThrows(BuildException.class, () -> plainJUnitResultFormatter.endTestSuite(new JUnitTest("Name")));
  }
}
