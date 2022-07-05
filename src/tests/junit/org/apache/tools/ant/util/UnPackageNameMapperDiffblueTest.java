package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class UnPackageNameMapperDiffblueTest {
  /**
  * Method under test: default or parameterless constructor of {@link UnPackageNameMapper}
  */
  @Test
  public void testConstructor() {
    // Arrange and Act
    UnPackageNameMapper actualUnPackageNameMapper = new UnPackageNameMapper();

    // Assert
    assertFalse(actualUnPackageNameMapper.getHandleDirSep());
    assertNull(actualUnPackageNameMapper.toPrefix);
    assertNull(actualUnPackageNameMapper.toPostfix);
    assertEquals(0, actualUnPackageNameMapper.prefixLength);
    assertEquals(0, actualUnPackageNameMapper.postfixLength);
    assertNull(actualUnPackageNameMapper.fromPrefix);
    assertNull(actualUnPackageNameMapper.fromPostfix);
  }

  /**
   * Method under test: {@link UnPackageNameMapper#extractVariablePart(String)}
   */
  @Test
  public void testExtractVariablePart() {
    // Arrange, Act and Assert
    assertEquals("Name", (new UnPackageNameMapper()).extractVariablePart("Name"));
  }
}

