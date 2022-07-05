package org.apache.tools.ant.util;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.lang.reflect.InvocationTargetException;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class ReflectUtilDiffblueTest {
  /**
  * Method under test: {@link ReflectUtil#getField(Object, String)}
  */
  @Test
  public void testGetField() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> ReflectUtil.getField("Obj", "Field Name"));
  }

  /**
   * Method under test: {@link ReflectUtil#invoke(Object, String)}
   */
  @Test
  public void testInvoke() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> ReflectUtil.invoke("Obj", "Method Name"));
    assertThrows(BuildException.class, () -> ReflectUtil.invoke("Obj", "Method Name", Object.class, "Arg"));
  }

  /**
   * Method under test: {@link ReflectUtil#invoke(Object, String, Class, Object, Class, Object)}
   */
  @Test
  public void testInvoke2() {
    // Arrange
    Class<Object> argType1 = Object.class;

    // Act and Assert
    assertThrows(BuildException.class,
        () -> ReflectUtil.invoke("Obj", "Method Name", argType1, "Arg1", Object.class, "Arg2"));
  }

  /**
   * Method under test: {@link ReflectUtil#invokeStatic(Object, String)}
   */
  @Test
  public void testInvokeStatic() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> ReflectUtil.invokeStatic(Object.class, "foo"));
  }

  /**
   * Method under test: {@link ReflectUtil#newInstance(Class, Class[], Object[])}
   */
  @Test
  public void testNewInstance() {
    // Arrange
    Class<Object> ofClass = Object.class;

    // Act and Assert
    assertThrows(BuildException.class,
        () -> ReflectUtil.newInstance(ofClass, new Class[]{Object.class}, new Object[]{"Args"}));
  }

  /**
   * Method under test: {@link ReflectUtil#newInstance(Class, Class[], Object[])}
   */
  @Test
  public void testNewInstance2() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class,
        () -> ReflectUtil.newInstance(Object.class, new Class[]{}, new Object[]{"Args"}));
  }

  /**
   * Method under test: {@link ReflectUtil#newInstance(Class, Class[], Object[])}
   */
  @Test
  public void testNewInstance3() {
    // Arrange, Act and Assert
    assertTrue(ReflectUtil.newInstance(BuildException.class, new Class[]{}, new Object[]{}) instanceof BuildException);
  }

  /**
   * Method under test: {@link ReflectUtil#respondsTo(Object, String)}
   */
  @Test
  public void testRespondsTo() throws BuildException {
    // Arrange, Act and Assert
    assertFalse(ReflectUtil.respondsTo("42", "Method Name"));
  }

  /**
   * Method under test: {@link ReflectUtil#throwBuildException(Exception)}
   */
  @Test
  public void testThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> ReflectUtil.throwBuildException(new Exception("foo")));
    assertThrows(BuildException.class,
        () -> ReflectUtil.throwBuildException(new InvocationTargetException(new Throwable())));
  }
}

