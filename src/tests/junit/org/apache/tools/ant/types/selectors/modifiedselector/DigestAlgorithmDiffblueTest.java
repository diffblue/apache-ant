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

public class DigestAlgorithmDiffblueTest {
  /**
   * Test {@link DigestAlgorithm#setAlgorithm(String)}.
   * <ul>
   *   <li>When {@code Algorithm}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DigestAlgorithm#setAlgorithm(String)}
   */
  @Test
  public void testSetAlgorithm_whenAlgorithm() {
    // Arrange
    DigestAlgorithm digestAlgorithm = new DigestAlgorithm();

    // Act
    digestAlgorithm.setAlgorithm("Algorithm");

    // Assert
    assertFalse(digestAlgorithm.isValid());
  }

  /**
   * Test {@link DigestAlgorithm#setAlgorithm(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DigestAlgorithm#setAlgorithm(String)}
   */
  @Test
  public void testSetAlgorithm_whenNull() {
    // Arrange
    DigestAlgorithm digestAlgorithm = new DigestAlgorithm();

    // Act
    digestAlgorithm.setAlgorithm(null);

    // Assert
    assertFalse(digestAlgorithm.isValid());
  }

  /**
   * Test {@link DigestAlgorithm#initMessageDigest()}.
   * <ul>
   *   <li>Given {@link DigestAlgorithm} (default constructor) Algorithm is {@code Algorithm}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DigestAlgorithm#initMessageDigest()}
   */
  @Test
  public void testInitMessageDigest_givenDigestAlgorithmAlgorithmIsAlgorithm() {
    // Arrange
    DigestAlgorithm digestAlgorithm = new DigestAlgorithm();
    digestAlgorithm.setAlgorithm("Algorithm");

    // Act and Assert
    assertThrows(BuildException.class, () -> digestAlgorithm.initMessageDigest());
  }

  /**
   * Test {@link DigestAlgorithm#initMessageDigest()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DigestAlgorithm#initMessageDigest()}
   */
  @Test
  public void testInitMessageDigest_thenThrowBuildException() {
    // Arrange
    DigestAlgorithm digestAlgorithm = new DigestAlgorithm();
    digestAlgorithm.setProvider("Provider");

    // Act and Assert
    assertThrows(BuildException.class, () -> digestAlgorithm.initMessageDigest());
  }

  /**
   * Test {@link DigestAlgorithm#isValid()}.
   * <ul>
   *   <li>Given {@link DigestAlgorithm} (default constructor) Algorithm is {@code Algorithm}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DigestAlgorithm#isValid()}
   */
  @Test
  public void testIsValid_givenDigestAlgorithmAlgorithmIsAlgorithm_thenReturnFalse() {
    // Arrange
    DigestAlgorithm digestAlgorithm = new DigestAlgorithm();
    digestAlgorithm.setAlgorithm("Algorithm");

    // Act and Assert
    assertFalse(digestAlgorithm.isValid());
  }

  /**
   * Test {@link DigestAlgorithm#isValid()}.
   * <ul>
   *   <li>Given {@link DigestAlgorithm} (default constructor) Algorithm is {@code SHA}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DigestAlgorithm#isValid()}
   */
  @Test
  public void testIsValid_givenDigestAlgorithmAlgorithmIsSha_thenReturnTrue() {
    // Arrange
    DigestAlgorithm digestAlgorithm = new DigestAlgorithm();
    digestAlgorithm.setAlgorithm("SHA");

    // Act and Assert
    assertTrue(digestAlgorithm.isValid());
  }

  /**
   * Test {@link DigestAlgorithm#isValid()}.
   * <ul>
   *   <li>Given {@link DigestAlgorithm} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DigestAlgorithm#isValid()}
   */
  @Test
  public void testIsValid_givenDigestAlgorithm_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new DigestAlgorithm()).isValid());
  }

  /**
   * Test {@link DigestAlgorithm#getValue(File)}.
   * <ul>
   *   <li>Given {@link DigestAlgorithm} (default constructor) Algorithm is {@code Algorithm}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DigestAlgorithm#getValue(File)}
   */
  @Test
  public void testGetValue_givenDigestAlgorithmAlgorithmIsAlgorithm_thenThrowBuildException() {
    // Arrange
    DigestAlgorithm digestAlgorithm = new DigestAlgorithm();
    digestAlgorithm.setAlgorithm("Algorithm");

    // Act and Assert
    assertThrows(BuildException.class,
        () -> digestAlgorithm.getValue(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link DigestAlgorithm#getValue(File)}.
   * <ul>
   *   <li>Given {@link DigestAlgorithm} (default constructor) Provider is empty string.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DigestAlgorithm#getValue(File)}
   */
  @Test
  public void testGetValue_givenDigestAlgorithmProviderIsEmptyString_thenReturnNull() {
    // Arrange
    DigestAlgorithm digestAlgorithm = new DigestAlgorithm();
    digestAlgorithm.setProvider("");

    // Act and Assert
    assertNull(digestAlgorithm.getValue(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link DigestAlgorithm#getValue(File)}.
   * <ul>
   *   <li>Given {@link DigestAlgorithm} (default constructor) Provider is {@code null}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DigestAlgorithm#getValue(File)}
   */
  @Test
  public void testGetValue_givenDigestAlgorithmProviderIsNull_thenReturnNull() {
    // Arrange
    DigestAlgorithm digestAlgorithm = new DigestAlgorithm();
    digestAlgorithm.setProvider("null");

    // Act and Assert
    assertNull(digestAlgorithm.getValue(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link DigestAlgorithm#getValue(File)}.
   * <ul>
   *   <li>Given {@link DigestAlgorithm} (default constructor) Provider is {@code Provider}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DigestAlgorithm#getValue(File)}
   */
  @Test
  public void testGetValue_givenDigestAlgorithmProviderIsProvider_thenThrowBuildException() {
    // Arrange
    DigestAlgorithm digestAlgorithm = new DigestAlgorithm();
    digestAlgorithm.setProvider("Provider");

    // Act and Assert
    assertThrows(BuildException.class,
        () -> digestAlgorithm.getValue(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link DigestAlgorithm#getValue(File)}.
   * <ul>
   *   <li>Given {@link DigestAlgorithm} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DigestAlgorithm#getValue(File)}
   */
  @Test
  public void testGetValue_givenDigestAlgorithm_thenReturnNull() {
    // Arrange
    DigestAlgorithm digestAlgorithm = new DigestAlgorithm();

    // Act and Assert
    assertNull(digestAlgorithm.getValue(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link DigestAlgorithm#getValue(File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code null} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link DigestAlgorithm#getValue(File)}
   */
  @Test
  public void testGetValue_whenPropertyIsJavaIoTmpdirIsNullToFile() {
    // Arrange
    DigestAlgorithm digestAlgorithm = new DigestAlgorithm();
    digestAlgorithm.setProvider("Provider");

    // Act and Assert
    assertNull(digestAlgorithm.getValue(Paths.get(System.getProperty("java.io.tmpdir"), "null").toFile()));
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link DigestAlgorithm}
   *   <li>{@link DigestAlgorithm#setProvider(String)}
   *   <li>{@link DigestAlgorithm#toString()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    DigestAlgorithm actualDigestAlgorithm = new DigestAlgorithm();
    actualDigestAlgorithm.setProvider("Provider");

    // Assert
    assertEquals("<DigestAlgorithm:algorithm=MD5;provider=Provider>", actualDigestAlgorithm.toString());
  }
}
