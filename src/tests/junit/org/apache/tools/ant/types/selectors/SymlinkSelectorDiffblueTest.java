package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.assertFalse;
import java.io.File;
import java.nio.file.Paths;
import org.junit.Test;

public class SymlinkSelectorDiffblueTest {
  /**
   * Test {@link SymlinkSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SymlinkSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_whenNull() {
    // Arrange
    SymlinkSelector symlinkSelector = new SymlinkSelector();

    // Act and Assert
    assertFalse(symlinkSelector.isSelected(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(),
        "foo.txt", null));
  }

  /**
   * Test {@link SymlinkSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link SymlinkSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_whenPropertyIsJavaIoTmpdirIsTestTxtToFile() {
    // Arrange
    SymlinkSelector symlinkSelector = new SymlinkSelector();
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(symlinkSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }
}
