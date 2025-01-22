package org.apache.tools.ant.taskdefs.optional.depend.constantpool;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class InterfaceMethodRefCPInfoDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link InterfaceMethodRefCPInfo}
   *   <li>{@link InterfaceMethodRefCPInfo#getInterfaceMethodClassName()}
   *   <li>{@link InterfaceMethodRefCPInfo#getInterfaceMethodName()}
   *   <li>{@link InterfaceMethodRefCPInfo#getInterfaceMethodType()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    InterfaceMethodRefCPInfo actualInterfaceMethodRefCPInfo = new InterfaceMethodRefCPInfo();
    String actualInterfaceMethodClassName = actualInterfaceMethodRefCPInfo.getInterfaceMethodClassName();
    String actualInterfaceMethodName = actualInterfaceMethodRefCPInfo.getInterfaceMethodName();

    // Assert
    assertNull(actualInterfaceMethodClassName);
    assertNull(actualInterfaceMethodName);
    assertNull(actualInterfaceMethodRefCPInfo.getInterfaceMethodType());
    assertEquals(1, actualInterfaceMethodRefCPInfo.getNumEntries());
    assertFalse(actualInterfaceMethodRefCPInfo.isResolved());
    assertEquals(ConstantPoolEntry.CONSTANT_INTERFACEMETHODREF, actualInterfaceMethodRefCPInfo.getTag());
  }

  /**
   * Test {@link InterfaceMethodRefCPInfo#toString()}.
   * <p>
   * Method under test: {@link InterfaceMethodRefCPInfo#toString()}
   */
  @Test
  public void testToString() {
    // Arrange, Act and Assert
    assertEquals("InterfaceMethod : Class index = 0, name and type index = 0",
        (new InterfaceMethodRefCPInfo()).toString());
  }
}
