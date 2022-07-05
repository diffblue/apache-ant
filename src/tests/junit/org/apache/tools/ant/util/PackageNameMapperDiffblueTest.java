package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class PackageNameMapperDiffblueTest {
  /**
  * Method under test: default or parameterless constructor of {@link PackageNameMapper}
  */
  @Test
  public void testConstructor() {
    // Arrange and Act
    PackageNameMapper actualPackageNameMapper = new PackageNameMapper();

    // Assert
    assertFalse(actualPackageNameMapper.getHandleDirSep());
    assertNull(actualPackageNameMapper.toPrefix);
    assertNull(actualPackageNameMapper.toPostfix);
    assertEquals(0, actualPackageNameMapper.prefixLength);
    assertEquals(0, actualPackageNameMapper.postfixLength);
    assertNull(actualPackageNameMapper.fromPrefix);
    assertNull(actualPackageNameMapper.fromPostfix);
  }

  /**
   * Method under test: {@link PackageNameMapper#extractVariablePart(String)}
   */
  @Test
  public void testExtractVariablePart() {
    // Arrange, Act and Assert
    assertEquals("Name", (new PackageNameMapper()).extractVariablePart("Name"));
  }

  /**
   * Method under test: {@link PackageNameMapper#extractVariablePart(String)}
   */
  @Test
  public void testExtractVariablePart2() {
    // Arrange
    PackageNameMapper packageNameMapper = new PackageNameMapper();
    packageNameMapper.setHandleDirSep(true);

    // Act and Assert
    assertEquals("Name", packageNameMapper.extractVariablePart("Name"));
  }
}

