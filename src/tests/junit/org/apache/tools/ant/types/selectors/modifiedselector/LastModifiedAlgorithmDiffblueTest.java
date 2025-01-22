package org.apache.tools.ant.types.selectors.modifiedselector;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.junit.Test;

public class LastModifiedAlgorithmDiffblueTest {
  /**
   * Test {@link LastModifiedAlgorithm#getValue(File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code 42} and {@code 42} toFile.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link LastModifiedAlgorithm#getValue(File)}
   */
  @Test
  public void testGetValue_whenPropertyIsJavaIoTmpdirIs42And42ToFile_thenReturnNull() {
    // Arrange
    LastModifiedAlgorithm lastModifiedAlgorithm = new LastModifiedAlgorithm();

    // Act and Assert
    assertNull(
        lastModifiedAlgorithm.getValue(Paths.get(System.getProperty("java.io.tmpdir"), "42", "42", "foo").toFile()));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link LastModifiedAlgorithm}
   *   <li>{@link LastModifiedAlgorithm#isValid()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange, Act and Assert
    assertTrue((new LastModifiedAlgorithm()).isValid());
  }
}
