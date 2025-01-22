package org.apache.tools.ant.taskdefs.optional.native2ascii;

import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.ProjectComponent;
import org.apache.tools.ant.taskdefs.optional.Native2Ascii;
import org.apache.tools.ant.types.Commandline;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class KaffeNative2AsciiDiffblueTest {
  /**
   * Test {@link KaffeNative2Ascii#setup(Commandline, Native2Ascii)}.
   * <ul>
   *   <li>Given {@code true}.</li>
   *   <li>When {@link Native2Ascii} (default constructor) Reverse is {@code true}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link KaffeNative2Ascii#setup(Commandline, Native2Ascii)}
   */
  @Test
  public void testSetup_givenTrue_whenNative2AsciiReverseIsTrue_thenThrowBuildException() throws BuildException {
    // Arrange
    KaffeNative2Ascii kaffeNative2Ascii = new KaffeNative2Ascii();
    Commandline cmd = new Commandline("To Process");

    Native2Ascii args = new Native2Ascii();
    args.setReverse(true);

    // Act and Assert
    assertThrows(BuildException.class, () -> kaffeNative2Ascii.setup(cmd, args));
  }

  /**
   * Test {@link KaffeNative2Ascii#run(Commandline, ProjectComponent)}.
   * <p>
   * Method under test: {@link KaffeNative2Ascii#run(Commandline, ProjectComponent)}
   */
  @Test
  public void testRun() throws BuildException {
    // Arrange
    KaffeNative2Ascii kaffeNative2Ascii = new KaffeNative2Ascii();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> kaffeNative2Ascii.run(new Commandline("To Process"), Path.systemBootClasspath));
  }
}
