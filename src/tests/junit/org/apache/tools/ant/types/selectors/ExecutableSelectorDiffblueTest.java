package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.assertFalse;
import java.io.File;
import java.nio.file.Paths;
import org.junit.Test;

public class ExecutableSelectorDiffblueTest {
  /**
   * Test {@link ExecutableSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ExecutableSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_whenNull_thenReturnFalse() {
    // Arrange
    ExecutableSelector executableSelector = new ExecutableSelector();

    // Act and Assert
    assertFalse(executableSelector.isSelected(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(),
        "foo.txt", null));
  }
}
