package org.apache.tools.ant.property;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.PropertyHelper;
import org.junit.Test;

public class LocalPropertyStackDiffblueTest {
  /**
   * Test {@link LocalPropertyStack#copy()}.
   * <p>
   * Method under test: {@link LocalPropertyStack#copy()}
   */
  @Test
  public void testCopy() {
    // Arrange, Act and Assert
    assertTrue((new LocalPropertyStack()).copy().getPropertyNames().isEmpty());
  }

  /**
   * Test {@link LocalPropertyStack#evaluate(String, PropertyHelper)}.
   * <p>
   * Method under test: {@link LocalPropertyStack#evaluate(String, PropertyHelper)}
   */
  @Test
  public void testEvaluate() {
    // Arrange, Act and Assert
    assertNull((new LocalPropertyStack()).evaluate("Property", null));
  }

  /**
   * Test {@link LocalPropertyStack#setNew(String, Object, PropertyHelper)}.
   * <p>
   * Method under test: {@link LocalPropertyStack#setNew(String, Object, PropertyHelper)}
   */
  @Test
  public void testSetNew() {
    // Arrange, Act and Assert
    assertFalse((new LocalPropertyStack()).setNew("Property", "Value", null));
  }

  /**
   * Test {@link LocalPropertyStack#set(String, Object, PropertyHelper)}.
   * <p>
   * Method under test: {@link LocalPropertyStack#set(String, Object, PropertyHelper)}
   */
  @Test
  public void testSet() {
    // Arrange, Act and Assert
    assertFalse((new LocalPropertyStack()).set("Property", "Value", null));
  }

  /**
   * Test {@link LocalPropertyStack#getPropertyNames()}.
   * <p>
   * Method under test: {@link LocalPropertyStack#getPropertyNames()}
   */
  @Test
  public void testGetPropertyNames() {
    // Arrange, Act and Assert
    assertTrue((new LocalPropertyStack()).getPropertyNames().isEmpty());
  }

  /**
   * Test new {@link LocalPropertyStack} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link LocalPropertyStack}
   */
  @Test
  public void testNewLocalPropertyStack() {
    // Arrange, Act and Assert
    assertTrue((new LocalPropertyStack()).getPropertyNames().isEmpty());
  }
}
