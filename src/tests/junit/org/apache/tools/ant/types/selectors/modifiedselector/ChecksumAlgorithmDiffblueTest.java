package org.apache.tools.ant.types.selectors.modifiedselector;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class ChecksumAlgorithmDiffblueTest {
  /**
   * Test {@link ChecksumAlgorithm#setAlgorithm(String)}.
   * <ul>
   *   <li>When {@code Algorithm}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ChecksumAlgorithm#setAlgorithm(String)}
   */
  @Test
  public void testSetAlgorithm_whenAlgorithm() {
    // Arrange
    ChecksumAlgorithm checksumAlgorithm = new ChecksumAlgorithm();

    // Act
    checksumAlgorithm.setAlgorithm("Algorithm");

    // Assert
    assertFalse(checksumAlgorithm.isValid());
  }

  /**
   * Test {@link ChecksumAlgorithm#setAlgorithm(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ChecksumAlgorithm#setAlgorithm(String)}
   */
  @Test
  public void testSetAlgorithm_whenNull() {
    // Arrange
    ChecksumAlgorithm checksumAlgorithm = new ChecksumAlgorithm();

    // Act
    checksumAlgorithm.setAlgorithm(null);

    // Assert
    assertFalse(checksumAlgorithm.isValid());
  }

  /**
   * Test {@link ChecksumAlgorithm#initChecksum()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ChecksumAlgorithm#initChecksum()}
   */
  @Test
  public void testInitChecksum_thenThrowBuildException() {
    // Arrange
    ChecksumAlgorithm checksumAlgorithm = new ChecksumAlgorithm();
    checksumAlgorithm.setAlgorithm("Algorithm");

    // Act and Assert
    assertThrows(BuildException.class, () -> checksumAlgorithm.initChecksum());
  }

  /**
   * Test {@link ChecksumAlgorithm#isValid()}.
   * <ul>
   *   <li>Given {@link ChecksumAlgorithm} (default constructor) Algorithm is {@code ADLER}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ChecksumAlgorithm#isValid()}
   */
  @Test
  public void testIsValid_givenChecksumAlgorithmAlgorithmIsAdler_thenReturnTrue() {
    // Arrange
    ChecksumAlgorithm checksumAlgorithm = new ChecksumAlgorithm();
    checksumAlgorithm.setAlgorithm("ADLER");

    // Act and Assert
    assertTrue(checksumAlgorithm.isValid());
  }

  /**
   * Test {@link ChecksumAlgorithm#isValid()}.
   * <ul>
   *   <li>Given {@link ChecksumAlgorithm} (default constructor) Algorithm is {@code Algorithm}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ChecksumAlgorithm#isValid()}
   */
  @Test
  public void testIsValid_givenChecksumAlgorithmAlgorithmIsAlgorithm_thenReturnFalse() {
    // Arrange
    ChecksumAlgorithm checksumAlgorithm = new ChecksumAlgorithm();
    checksumAlgorithm.setAlgorithm("Algorithm");

    // Act and Assert
    assertFalse(checksumAlgorithm.isValid());
  }

  /**
   * Test {@link ChecksumAlgorithm#isValid()}.
   * <ul>
   *   <li>Given {@link ChecksumAlgorithm} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ChecksumAlgorithm#isValid()}
   */
  @Test
  public void testIsValid_givenChecksumAlgorithm_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new ChecksumAlgorithm()).isValid());
  }

  /**
   * Test {@link ChecksumAlgorithm#getValue(File)}.
   * <ul>
   *   <li>Given {@link ChecksumAlgorithm} (default constructor) Algorithm is {@code ADLER}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ChecksumAlgorithm#getValue(File)}
   */
  @Test
  public void testGetValue_givenChecksumAlgorithmAlgorithmIsAdler_thenReturnNull() {
    // Arrange
    ChecksumAlgorithm checksumAlgorithm = new ChecksumAlgorithm();
    checksumAlgorithm.setAlgorithm("ADLER");

    // Act and Assert
    assertNull(checksumAlgorithm.getValue(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link ChecksumAlgorithm#getValue(File)}.
   * <ul>
   *   <li>Given {@link ChecksumAlgorithm} (default constructor) Algorithm is {@code Algorithm}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ChecksumAlgorithm#getValue(File)}
   */
  @Test
  public void testGetValue_givenChecksumAlgorithmAlgorithmIsAlgorithm_thenThrowBuildException() {
    // Arrange
    ChecksumAlgorithm checksumAlgorithm = new ChecksumAlgorithm();
    checksumAlgorithm.setAlgorithm("Algorithm");

    // Act and Assert
    assertThrows(BuildException.class,
        () -> checksumAlgorithm.getValue(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link ChecksumAlgorithm#getValue(File)}.
   * <ul>
   *   <li>Given {@link ChecksumAlgorithm} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ChecksumAlgorithm#getValue(File)}
   */
  @Test
  public void testGetValue_givenChecksumAlgorithm_thenReturnNull() {
    // Arrange
    ChecksumAlgorithm checksumAlgorithm = new ChecksumAlgorithm();

    // Act and Assert
    assertNull(checksumAlgorithm.getValue(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link ChecksumAlgorithm#getValue(File)}.
   * <ul>
   *   <li>Given {@link ChecksumAlgorithm} (default constructor).</li>
   *   <li>When Property is {@code java.io.tmpdir} is {@code CRC} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link ChecksumAlgorithm#getValue(File)}
   */
  @Test
  public void testGetValue_givenChecksumAlgorithm_whenPropertyIsJavaIoTmpdirIsCrcToFile() {
    // Arrange
    ChecksumAlgorithm checksumAlgorithm = new ChecksumAlgorithm();

    // Act and Assert
    assertNull(checksumAlgorithm.getValue(Paths.get(System.getProperty("java.io.tmpdir"), "CRC").toFile()));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link ChecksumAlgorithm}
   *   <li>{@link ChecksumAlgorithm#toString()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange, Act and Assert
    assertEquals("<ChecksumAlgorithm:algorithm=CRC>", (new ChecksumAlgorithm()).toString());
  }
}
