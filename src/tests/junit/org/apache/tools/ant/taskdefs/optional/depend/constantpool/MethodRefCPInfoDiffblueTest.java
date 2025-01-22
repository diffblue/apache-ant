package org.apache.tools.ant.taskdefs.optional.depend.constantpool;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class MethodRefCPInfoDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link MethodRefCPInfo}
   *   <li>{@link MethodRefCPInfo#getMethodClassName()}
   *   <li>{@link MethodRefCPInfo#getMethodName()}
   *   <li>{@link MethodRefCPInfo#getMethodType()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    MethodRefCPInfo actualMethodRefCPInfo = new MethodRefCPInfo();
    String actualMethodClassName = actualMethodRefCPInfo.getMethodClassName();
    String actualMethodName = actualMethodRefCPInfo.getMethodName();

    // Assert
    assertNull(actualMethodClassName);
    assertNull(actualMethodName);
    assertNull(actualMethodRefCPInfo.getMethodType());
    assertEquals(1, actualMethodRefCPInfo.getNumEntries());
    assertFalse(actualMethodRefCPInfo.isResolved());
    assertEquals(ConstantPoolEntry.CONSTANT_METHODREF, actualMethodRefCPInfo.getTag());
  }

  /**
   * Test {@link MethodRefCPInfo#toString()}.
   * <p>
   * Method under test: {@link MethodRefCPInfo#toString()}
   */
  @Test
  public void testToString() {
    // Arrange, Act and Assert
    assertEquals("Method : Class index = 0, name and type index = 0", (new MethodRefCPInfo()).toString());
  }
}
