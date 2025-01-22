package org.apache.tools.ant.types.selectors.modifiedselector;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.junit.Test;

public class HashvalueAlgorithmDiffblueTest {
  /**
   * Test {@link HashvalueAlgorithm#getValue(File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code 42} and {@code 42} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link HashvalueAlgorithm#getValue(File)}
   */
  @Test
  public void testGetValue_whenPropertyIsJavaIoTmpdirIs42And42ToFile() {
    // Arrange
    HashvalueAlgorithm hashvalueAlgorithm = new HashvalueAlgorithm();

    // Act and Assert
    assertNull(
        hashvalueAlgorithm.getValue(Paths.get(System.getProperty("java.io.tmpdir"), "42", "42", "foo").toFile()));
  }

  /**
   * Test {@link HashvalueAlgorithm#getValue(File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link HashvalueAlgorithm#getValue(File)}
   */
  @Test
  public void testGetValue_whenPropertyIsJavaIoTmpdirIsTestTxtToFile() {
    // Arrange
    HashvalueAlgorithm hashvalueAlgorithm = new HashvalueAlgorithm();

    // Act and Assert
    assertNull(hashvalueAlgorithm.getValue(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link HashvalueAlgorithm}
   *   <li>{@link HashvalueAlgorithm#toString()}
   *   <li>{@link HashvalueAlgorithm#isValid()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    HashvalueAlgorithm actualHashvalueAlgorithm = new HashvalueAlgorithm();
    String actualToStringResult = actualHashvalueAlgorithm.toString();

    // Assert
    assertEquals("HashvalueAlgorithm", actualToStringResult);
    assertTrue(actualHashvalueAlgorithm.isValid());
  }
}
