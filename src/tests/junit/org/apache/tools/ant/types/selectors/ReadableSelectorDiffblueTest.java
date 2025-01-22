package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.junit.Test;

public class ReadableSelectorDiffblueTest {
  /**
   * Test {@link ReadableSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReadableSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_thenReturnTrue() {
    // Arrange
    ReadableSelector readableSelector = new ReadableSelector();
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(readableSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link ReadableSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ReadableSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_whenNull_thenReturnFalse() {
    // Arrange
    ReadableSelector readableSelector = new ReadableSelector();

    // Act and Assert
    assertFalse(readableSelector.isSelected(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(),
        "foo.txt", null));
  }
}
