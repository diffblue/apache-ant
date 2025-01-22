package org.apache.tools.ant.taskdefs.optional.native2ascii;

import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.ProjectComponent;
import org.apache.tools.ant.types.Commandline;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class SunNative2AsciiDiffblueTest {
  /**
   * Test {@link SunNative2Ascii#run(Commandline, ProjectComponent)}.
   * <p>
   * Method under test: {@link SunNative2Ascii#run(Commandline, ProjectComponent)}
   */
  @Test
  public void testRun() throws BuildException {
    // Arrange
    SunNative2Ascii sunNative2Ascii = new SunNative2Ascii();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> sunNative2Ascii.run(new Commandline("To Process"), Path.systemBootClasspath));
  }
}
