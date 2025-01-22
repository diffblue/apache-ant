package org.apache.tools.ant.taskdefs.optional.depend.constantpool;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class ConstantCPInfoDiffblueTest {
  /**
   * Test {@link ConstantCPInfo#getValue()}.
   * <p>
   * Method under test: {@link ConstantCPInfo#getValue()}
   */
  @Test
  public void testGetValue() {
    // Arrange, Act and Assert
    assertNull((new DoubleCPInfo()).getValue());
  }

  /**
   * Test {@link ConstantCPInfo#setValue(Object)}.
   * <p>
   * Method under test: {@link ConstantCPInfo#setValue(Object)}
   */
  @Test
  public void testSetValue() {
    // Arrange
    DoubleCPInfo doubleCPInfo = new DoubleCPInfo();

    // Act
    doubleCPInfo.setValue("New Value");

    // Assert
    assertEquals("New Value", doubleCPInfo.getValue());
  }
}
