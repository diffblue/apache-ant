package org.apache.tools.ant.util;

import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class ScriptFixBSFPathDiffblueTest {
  /**
   * Test {@link ScriptFixBSFPath#fixClassLoader(ClassLoader, String)}.
   * <ul>
   *   <li>When {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ScriptFixBSFPath#fixClassLoader(ClassLoader, String)}
   */
  @Test
  public void testFixClassLoader_whenAntClassLoader_thenThrowBuildException() {
    // Arrange
    ScriptFixBSFPath scriptFixBSFPath = new ScriptFixBSFPath();

    // Act and Assert
    assertThrows(BuildException.class, () -> scriptFixBSFPath.fixClassLoader(new AntClassLoader(), "en"));
  }
}
