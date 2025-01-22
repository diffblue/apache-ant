package org.apache.tools.zip;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import org.apache.tools.zip.UnsupportedZipFeatureException.Feature;
import org.junit.Test;

public class UnsupportedZipFeatureExceptionDiffblueTest {
  /**
   * Test {@link UnsupportedZipFeatureException#UnsupportedZipFeatureException(Feature, ZipEntry)}.
   * <p>
   * Method under test: {@link UnsupportedZipFeatureException#UnsupportedZipFeatureException(Feature, ZipEntry)}
   */
  @Test
  public void testNewUnsupportedZipFeatureException() {
    // Arrange
    ZipEntry entry = new ZipEntry();

    // Act
    UnsupportedZipFeatureException actualUnsupportedZipFeatureException = new UnsupportedZipFeatureException(null,
        entry);

    // Assert
    assertEquals("unsupported feature null used in entry ", actualUnsupportedZipFeatureException.getLocalizedMessage());
    assertEquals("unsupported feature null used in entry ", actualUnsupportedZipFeatureException.getMessage());
    assertNull(actualUnsupportedZipFeatureException.getCause());
    assertNull(actualUnsupportedZipFeatureException.getFeature());
    assertEquals(0, actualUnsupportedZipFeatureException.getSuppressed().length);
    assertSame(entry, actualUnsupportedZipFeatureException.getEntry());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link UnsupportedZipFeatureException#getEntry()}
   *   <li>{@link UnsupportedZipFeatureException#getFeature()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    ZipEntry entry = new ZipEntry();
    UnsupportedZipFeatureException unsupportedZipFeatureException = new UnsupportedZipFeatureException(null, entry);

    // Act
    ZipEntry actualEntry = unsupportedZipFeatureException.getEntry();

    // Assert
    assertNull(unsupportedZipFeatureException.getFeature());
    assertSame(entry, actualEntry);
  }
}
