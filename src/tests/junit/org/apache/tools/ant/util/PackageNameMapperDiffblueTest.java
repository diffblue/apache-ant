package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import org.junit.Test;

public class PackageNameMapperDiffblueTest {
  /**
   * Test {@link PackageNameMapper#extractVariablePart(String)}.
   * <ul>
   *   <li>Given {@link PackageNameMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link PackageNameMapper#extractVariablePart(String)}
   */
  @Test
  public void testExtractVariablePart_givenPackageNameMapper() {
    // Arrange, Act and Assert
    assertEquals("Name", (new PackageNameMapper()).extractVariablePart("Name"));
  }

  /**
   * Test {@link PackageNameMapper#extractVariablePart(String)}.
   * <ul>
   *   <li>Given {@link PackageNameMapper} (default constructor) HandleDirSep is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link PackageNameMapper#extractVariablePart(String)}
   */
  @Test
  public void testExtractVariablePart_givenPackageNameMapperHandleDirSepIsTrue() {
    // Arrange
    PackageNameMapper packageNameMapper = new PackageNameMapper();
    packageNameMapper.setHandleDirSep(true);

    // Act and Assert
    assertEquals("Name", packageNameMapper.extractVariablePart("Name"));
  }

  /**
   * Test new {@link PackageNameMapper} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link PackageNameMapper}
   */
  @Test
  public void testNewPackageNameMapper() {
    // Arrange, Act and Assert
    assertFalse((new PackageNameMapper()).getHandleDirSep());
  }
}
