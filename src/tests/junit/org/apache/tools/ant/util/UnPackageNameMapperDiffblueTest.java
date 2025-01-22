package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import org.junit.Test;

public class UnPackageNameMapperDiffblueTest {
  /**
   * Test {@link UnPackageNameMapper#extractVariablePart(String)}.
   * <p>
   * Method under test: {@link UnPackageNameMapper#extractVariablePart(String)}
   */
  @Test
  public void testExtractVariablePart() {
    // Arrange, Act and Assert
    assertEquals("Name", (new UnPackageNameMapper()).extractVariablePart("Name"));
  }

  /**
   * Test new {@link UnPackageNameMapper} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link UnPackageNameMapper}
   */
  @Test
  public void testNewUnPackageNameMapper() {
    // Arrange, Act and Assert
    assertFalse((new UnPackageNameMapper()).getHandleDirSep());
  }
}
