package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.assertFalse;
import java.io.File;
import java.nio.file.Paths;
import org.junit.Test;

public class WritableSelectorDiffblueTest {
  /**
   * Test {@link WritableSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WritableSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile_whenNull_thenReturnFalse() {
    // Arrange
    WritableSelector writableSelector = new WritableSelector();

    // Act and Assert
    assertFalse(writableSelector.isSelected(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(),
        "foo.txt", null));
  }
}
